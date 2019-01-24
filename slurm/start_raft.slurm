#!/bin/bash -l

#SBATCH -J raft
#SBATCH -N 1
#SBATCH -t 10:00:00
#SBATCH --exclusive
#SBATCH -o run/slurm_raft.out

## raft requires erlang21
module purge
module load erlang/21.2

main(){
    mkdir -p $WD
    setup_logging
    [[ -z $WD ]] && { log error "working dir not specified"; exit 1;}
    [[ -z $HOSTSFILE ]] && { log error "hostsfile not specified"; exit 1;}

    build_hostlist
    log info "Working directory: $WD"
    log info "HOSTSFILE: $HOSTSFILE"
    log info "Raft Hostlist $HOSTLISTSTRING"
    log info "Raft Hostlist ${HOSTLIST[*]}"

    export HOSTLISTSTRING

    # build releases
    declare -a RELEASE_NAMES
    declare -a RELEASE_PATHS

    rebar3 clean
    for i in `seq 0 $((SLURM_JOB_NUM_NODES-1))`;
    do
        RELEASE_NAMES[$i]=$(cut -d '@' -f1 <<< ${HOSTLIST[$i]} | tr -d \')
        RELEASE_PATHS[$i]="_build/${RELEASE_NAMES[$i]}/rel/slurm_raft/bin/slurm_raft"

        log info "Building node ${RELEASE_NAMES[$i]}"
        rebar3 as ${RELEASE_NAMES[$i]} release
    done


    # start cluster nodes
    for i in `seq 0 $((SLURM_JOB_NUM_NODES-1))`;
    do
        log info "Starting node ${RELEASE_NAMES[$i]}"
        clear_data_dir $i
        node_command $i start
    done
    log info "waiting a bit to ensure everything is started"
    sleep 2

    # connecting the cluster
    log info "Using node ${RELEASE_NAMES[0]} as leader"
    node_rpc_command 0 create ${HOSTLIST[@]}

    log info "Checking raft status"
    for i in `seq 0 $((SLURM_JOB_NUM_NODES-1))`;
    do
        log info "${RELEASE_NAMES[$i]}:"
        node_rpc_command $i status
    done

    log info "writing Hostlist: $HOSTLISTSTRING"
    echo $HOSTLISTSTRING >> $HOSTSFILE
    wait_for_benchmark_completion

    log info "Shutting raft down..."
    for i in `seq 0 $((SLURM_JOB_NUM_NODES-1))`;
    do
        log info "${RELEASE_NAMES[$i]}:"
        node_command $i stop
        clear_data_dir $i
    done
}

node_command() {
    local node_id=$1
    local cmd=$2

    local path=${RELEASE_PATHS[$node_id]}
    log info "Executing on node ${RELEASE_NAMES[$node_id]}: $path $cmd"

    srun -N1 -r$node_id bash -c "$path $cmd"
}

node_rpc_command() {
    local node_id=$1
    local cmd="${@:2}"

    node_command $node_id "rpc raft_console $cmd"
}

wait_for_benchmark_completion() {
    # basho bench removes the hostfile once it is done

    echo -n "$(tag info) waiting for basho bench to complete"
    timer=0
    while [[ -e $HOSTSFILE ]]; do
        ((timer++))
        # display status every 5 seconds
        if ((timer%5==0)); then
            echo -ne "."
        fi
        sleep 1
    done
    echo ": ok (${timer}s)"
}

build_hostlist() {
    local counter=0
    declare -a hosts
    NODELIST=$(scontrol show job $SLURM_JOBID | grep " NodeList" | awk -F= '{print $2}')
    for host in $(scontrol show hostnames $NODELIST); do
        counter=$(($counter+1))
        hosts+=("'node${counter}@${host}.zib.de'")
    done

    HOSTLIST=("${hosts[@]}")
    HOSTLISTSTRING=$(join_by , "${hosts[@]}")
}

function join_by { local IFS="$1"; shift; echo "$*"; }

clear_data_dir(){
    local node_id=$1
    log info "Clearing $RAFT_DATA_ROOT on node $node_id"
    srun -N1 -r$node_id bash -c "rm -r $RAFT_DATA_ROOT || true"
}

setup_logging(){
    LOGFILE="$WD/bbench-suite-$(date +%y.%m.%d-%H:%M:%S).log"
    log info "writing output also to $LOGFILE"
    # w/o -i option to tee, signal trapping does NOT work!
    exec &>> >(tee -i $LOGFILE)
}

log(){
    local level=$1
    local message=$2
    printf "%s %s\n" "$(tag $level)" "$message"
}

tag(){
    local level=$1
    printf "[raft] %s  [%s]" "$(date +%H:%M:%S)" "$level"
}

main

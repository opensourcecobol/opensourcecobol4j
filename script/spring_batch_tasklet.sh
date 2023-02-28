#!/bin/bash

while getopts c:t-: opt; do
    optarg="$OPTARG"
    if [[ "$opt" = - ]]; then
        opt="-${OPTARG%%=*}"
        optarg="${OPTARG/${OPTARG%%=*}/}"
        optarg="${optarg#=}"

        if [[ -z "$optarg" ]] && [[ ! "${!OPTIND}" = -* ]]; then
            optarg="${!OPTIND}"
            shift
        fi
    fi

    case "-$opt" in
        --target)
	        target="$optarg"
            ;;
        --class-name)
            class_name="$optarg"
            ;;
        --)
            break
            ;;
    esac
done
shift $((OPTIND - 1))

case "$target" in
    main-class-annotation)
        echo "@Component"
	    exit 0
	    ;;
    file-header)
        echo 'import org.slf4j.Logger;'
        echo 'import org.slf4j.LoggerFactory;'
        echo 'import org.springframework.batch.core.StepContribution;'
        echo 'import org.springframework.batch.core.scope.context.ChunkContext;'
        echo 'import org.springframework.batch.core.step.tasklet.Tasklet;'
        echo 'import org.springframework.batch.repeat.RepeatStatus;'
        echo 'import org.springframework.stereotype.Component;'
        exit 0
        ;;
    main-class-implements)
        echo -n 'Tasklet'
        exit 0
        ;;
    main-class-contents)
        echo '  @Override'
        echo '  public RepeatStatus execute(StepContribution conribution, ChunkContext chunkContext) throws Exception {'
        echo '    this.run();'
        echo '    return RepeatStatus.FINISHED;'
        echo '  }'
        exit 0
        ;;
esac

exit 1

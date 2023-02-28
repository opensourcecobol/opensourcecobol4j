#!/bin/bash

# This is an example of -edit-code-command.
# If you run the following command, cobj generates java source code compatible for Spring Batch.
# cobj -C -edit-code-command="bash spring_batch_tasklet.sh" <<<your COBOL source file>>>

# parse command line options with getopts
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
    # when `--target=main-class` is specified
    main-class-annotation)
        echo "@Component"
	    exit 0
	    ;;
    # when `--target=file-header` is specified
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
    # when `--target=main-class` is specified
    main-class-implements)
        echo -n 'Tasklet'
        exit 0
        ;;
    # when `--target=main-class` is specified
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

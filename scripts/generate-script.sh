#!/usr/bin/env bash

cat << EOF
BASE_DIR=$CI_BASE_DIR
RESULT_FTP_PATH=$CI_RESULT_FTP_PATH

AWS_PROFILE=$CI_AWS_PROFILE
AWS_TARGET=$CI_AWS_TARGET
EOF


cat << 'EOF'
TODAY=$(date +"%Y-%m-%d")
TODAY_DIR=$(date +"%Y%m%d")
NRO_SOURCE=$BASE_DIR/result/$TODAY_DIR/combined-stat

cd $BASE_DIR 

# If nro-stats.err is not empty, don't run the script
# This error file is generated from logger/logback 
if [ -s log/nro-stats.err ]; then
    echo "Error file from previous run is not empty, please take action!"
    exit
fi

java -Denvironment=production -Dconfig.file=./application.conf -jar nro-delegated-stats.jar 1>> log/nro-stats-$TODAY.log  2>> log/nro-stats.err

rsync -rtv $BASE_DIR/result/ $RESULT_FTP_PATH 1>> log/nro-stats-$TODAY.log  2>> log/nro-stats.err

/usr/local/bin/aws --profile $AWS_PROFILE s3 cp $NRO_SOURCE $AWS_TARGET   1>> log/nro-stats.log 2>> log/nro-stats.err
EOF

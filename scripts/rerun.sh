if [[ $# -lt 1 ]]; then 
	echo "Usage rerun.sh 2020-04-01"
	exit
fi
cd /home/app-admin/nro-stats
java -DstartDate=$1 -DendDate=$1 -Dconfig.file=./application.conf -jar nro-delegated-stats.jar 2>>log/nro-stats-$1.err 1>>log/nro-stats-$1.log


for file in ./domain-back-end/demo/uploads/*; 
do 
	
	filenameFull=$(basename $file)
	filenameWithoutExtension="${filenameFull%.*}"

	curl --location --request PUT 'http://127.0.0.1:8080/api/status' --header 'Content-Type: application/json' --data-raw '{ "fileName": "'$filenameFull'", "status": "PROCESSING" }'

	ffmpeg -i $file ./domain-back-end/demo/audios/$filenameWithoutExtension.mp3
	
	sleep 5
	curl --location --request PUT 'http://127.0.0.1:8080/api/status' --header 'Content-Type: application/json' --data-raw '{ "fileName": "'$filenameFull'", "status": "FINISHED" }'
	
	rm $file
done

for file in `ls -tr ./uploads/`;
do 
	
	filenameFull=$(basename $file)
	filenameWithoutExtension="${filenameFull%.*}"

	echo $filenameFull
	echo $filenameWithoutExtension

	curl --location --request PUT 'http://127.0.0.1:8080/api/status' --header 'Content-Type: application/json' --data-raw '{ "fileName": "'$filenameFull'", "status": "PROCESSING" }'

	ffmpeg -i ./uploads/$file ./audios/$filenameWithoutExtension.mp3
	
	sleep 5
	curl --location --request PUT 'http://127.0.0.1:8080/api/status' --header 'Content-Type: application/json' --data-raw '{ "fileName": "'$filenameFull'", "status": "FINISHED" }'
	
	rm ./uploads/$file
done


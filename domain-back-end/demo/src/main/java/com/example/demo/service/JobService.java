package com.example.demo.service;

import com.example.demo.domain.Job;
import com.example.demo.domain.Priority;
import com.example.demo.domain.Status;
import com.example.demo.dto.JobDTO;
import com.example.demo.repository.JobRepository;
import com.example.demo.util.MultipartUtility;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;

@Service
public class JobService {

    private final JobRepository jobRepository;
    private final Path folderForUploadedFiles = Paths.get("uploads");
    private final Path folderForFilesInQueue = Paths.get("queue");
    private final Path folderForFinishedFiles = Paths.get("audios");
    private final String brokingIP = "192.86.139.79";
    private final Integer threshold = 30; //MB

    public JobService(JobRepository jobRepository) {
        this.jobRepository = jobRepository;
    }

    public Long saveFile(MultipartFile file) {

//        String randomFileName = new Random().ints(97, 123)
//                .limit(10)
//                .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
//                .toString();

        // save the video
        try {
            Files.copy(file.getInputStream(), this.folderForUploadedFiles.resolve(file.getOriginalFilename()));
        } catch (Exception e) {
            if (e instanceof FileAlreadyExistsException) {
                throw new RuntimeException("A file of that name already exists.");
            }
            throw new RuntimeException(e.getMessage());
        }

        // create job

        Job newJob = new Job();
        newJob.setFileName(file.getOriginalFilename());
        newJob.setSize(file.getSize());
        newJob.setStatus(Status.WAITING);
        newJob.setPivot(false);

        Priority priority = null;
        try {
            URL url = new URL("http://"+brokingIP+":8080/api/jobs/priority?fileName="+file.getOriginalFilename());
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            con.setRequestMethod("GET");
            BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
            priority = Priority.valueOf(in.readLine());

            System.out.println("**********************");
            System.out.println(priority.equals(Priority.HIGH));
            System.out.println(getLoad());
            System.out.println(threshold*1000*1000);


            if (priority.equals(Priority.HIGH) && getLoad() >= threshold*1000*1000){
                System.out.println("here");
                newJob.setPivot(true);
                moveLowPriorityWaitingJobsToQueue();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        newJob.setPriority(priority);
        return jobRepository.save(newJob).getId();

    }

    private void moveLowPriorityWaitingJobsToQueue() throws IOException {
        List<Job> jobs = jobRepository.getAllByStatusAndPriority(Status.WAITING, Priority.LOW);

        for (Job job: jobs) {
            String fileName = job.getFileName();
            Path source = folderForUploadedFiles.resolve(fileName);
            Path target = folderForFilesInQueue.resolve(fileName);
            Files.move(source, target);
        }

    }

    private void moveLowPriorityWaitingJobsFromQueue() throws IOException {

        File folder = new File(folderForFilesInQueue.toString());
        File[] listOfFiles = folder.listFiles();

        for (File file : listOfFiles) {
            if (file.isFile()) {

                String fileName = file.getName();
                Path target = folderForUploadedFiles.resolve(fileName);

                Files.move(file.toPath(), target);
            }
        }

    }

    public Resource loadFile(Long jobId) throws IOException {
        Job job = jobRepository.getById(jobId);
        String fileName = job.getFileName();
        fileName = FilenameUtils.removeExtension(fileName) + ".mp3";

        try {
            Path file = folderForFinishedFiles.resolve(fileName);
            Resource resource = new UrlResource(file.toUri());

            if (resource.exists() || resource.isReadable()) {
                //Files.delete(file);
                return resource;
            } else
                throw new RuntimeException("Could not read the file!");

        } catch (MalformedURLException e) {
            throw new RuntimeException("Error: " + e.getMessage());
        }
    }

    public void changeStatus(String fileName, Status status) throws IOException {
        Job job = jobRepository.getByFileName(fileName);
        job.setStatus(status);

        if (job.getPivot() && status.equals(Status.FINISHED)){
            moveLowPriorityWaitingJobsFromQueue();
        }

        jobRepository.save(job);
    }

    public Status checkStatus(Long jobId) {
        return jobRepository.getById(jobId).getStatus();
    }

    public Integer getLoad() {
        List<Job> waitingJobs = jobRepository.getAllByStatus(Status.WAITING);
        int load = 0;
        for (Job job : waitingJobs)
            load += job.getSize();

        return load;
    }

    public void migrateWaitingLowPriorityJobs(String destinationIP, String fileName) {
        String charset = "UTF-8";
        File uploadFile = new File(folderForUploadedFiles+"/"+fileName);
        String requestURL = "http://"+destinationIP+":8080/api/files";

        try {
            MultipartUtility multipart = new MultipartUtility(requestURL, charset);

            multipart.addFilePart("file", uploadFile);

            multipart.finish();

        } catch (IOException ex) {
            System.err.println(ex);
        }

        jobRepository.deleteByFileName(fileName);

    }

    public List<JobDTO> getAllJobs(){
        List<JobDTO> jobs = new ArrayList<>();
        for (Job job: jobRepository.findAll()) {
            jobs.add(new JobDTO(job));
        }
        return jobs;
    }

}

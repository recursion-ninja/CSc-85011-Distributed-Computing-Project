package com.example.demo.service;

import com.example.demo.domain.Job;
import com.example.demo.domain.Status;
import com.example.demo.dto.JobDTO;
import com.example.demo.repository.JobRepository;
import com.example.demo.util.MultipartUtility;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.apache.commons.io.FilenameUtils;

@Service
public class JobService {

    private final JobRepository jobRepository;
    private final Path folderForUploadedFiles = Paths.get("uploads");
    private final Path folderForFinishedFiles = Paths.get("audios");

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
        //        newJob.setUserEmail();
//        newJob.setNumOfMinutes();
        return jobRepository.save(newJob).getId();

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

    public void changeStatus(String fileName, Status status) {
        Job job = jobRepository.getByFileName(fileName);
        job.setStatus(status);
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
}

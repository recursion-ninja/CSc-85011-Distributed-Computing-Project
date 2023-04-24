package com.example.demo.service;

import com.example.demo.domain.Job;
import com.example.demo.dto.JobDTO;
import com.example.demo.repository.JobRepository;
import org.springframework.stereotype.Service;

@Service
public class JobService {

    private final JobRepository jobRepository;

    public JobService(JobRepository jobRepository) {
        this.jobRepository = jobRepository;
    }

    public void createJob(JobDTO jobDTO){
        System.out.println("In Service");
        Job newJob = new Job();
        newJob.setMemorySize(jobDTO.getMemorySize());
        newJob.setDiskSpace(jobDTO.getDiskSpace());
        newJob.setNumOfCPUCores(jobDTO.getNumOfCPUCores());
        newJob.setNumOfGPUCores(jobDTO.getNumOfGPUCores());
        newJob.setNumOfMinutes(jobDTO.getNumOfMinutes());
        newJob.setUserEmail(jobDTO.getUserEmail());
        //newJob.setDomain();
        jobRepository.save(newJob);
    }
}

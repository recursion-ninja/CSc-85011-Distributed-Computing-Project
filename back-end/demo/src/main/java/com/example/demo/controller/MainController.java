package com.example.demo.controller;

import com.example.demo.dto.JobDTO;
import com.example.demo.service.JobService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
public class MainController {

    private final JobService jobService;

    public MainController(JobService jobService) {
        this.jobService = jobService;
    }

    @PostMapping("/jobs")
    public ResponseEntity createJob(@RequestBody JobDTO jobDTO){
        System.out.println("In Controller");
        jobService.createJob(jobDTO);
        return ResponseEntity.ok(null);
    }



}

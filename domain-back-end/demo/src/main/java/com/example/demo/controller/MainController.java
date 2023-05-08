package com.example.demo.controller;

import com.example.demo.domain.Status;
import com.example.demo.dto.JobDTO;
import com.example.demo.service.JobService;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.Map;

@RestController
@RequestMapping("/api")
public class MainController {

    private final JobService jobService;

    public MainController(JobService jobService) {
        this.jobService = jobService;
    }

    @PostMapping("/files")
    public ResponseEntity<Long> uploadVideo( @RequestPart("file") MultipartFile video) {

        Long jobId = jobService.saveFile(video);

        return ResponseEntity.ok(jobId);
    }

    @GetMapping("/files/{job-id}")
    public ResponseEntity<Resource> loadFile( @PathVariable("job-id") Long jobId) throws IOException {

        Resource script = jobService.loadFile(jobId);

        return ResponseEntity.ok().body(script);
    }

    @GetMapping("/status/{job-id}")
    public ResponseEntity<String> checkStatus(@PathVariable("job-id") Long jobId){
        Status status = jobService.checkStatus(jobId);
        return ResponseEntity.ok(status.toString());
    }

    @PutMapping("/status")
    public ResponseEntity<Void> changeStatus(@RequestBody Map<String, String> body){
        String fileName = body.get("fileName");
        Status status = Status.valueOf(body.get("status"));

        jobService.changeStatus( fileName, status );

        return ResponseEntity.ok().build();
    }

    @GetMapping("/load")
    public ResponseEntity<Integer> getLoad(){
        int load = jobService.getLoad();
        return ResponseEntity.ok(load);
    }

    @PostMapping("/migrate")
    public ResponseEntity migrateJob(@RequestBody Map<String, String> body) {
        String destinationDomainIP = body.get("destination");
        String fileName = body.get("fileName");

        jobService.migrateWaitingLowPriorityJobs(destinationDomainIP, fileName);
        return ResponseEntity.ok().build();
    }
}

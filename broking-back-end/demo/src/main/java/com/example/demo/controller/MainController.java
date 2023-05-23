package com.example.demo.controller;

import com.example.demo.domain.Priority;
import com.example.demo.dto.DomainDTO;
import com.example.demo.dto.JobDTO;
import com.example.demo.service.MainService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api")
public class MainController {

    private final MainService mainService;

    public MainController(MainService mainService) {
        this.mainService = mainService;
    }

    @PostMapping("/domains")
    public ResponseEntity<Void> addDomain(@RequestBody DomainDTO domainDTO){
        mainService.addDomain(domainDTO);
        return ResponseEntity.ok().build();
    }

    @GetMapping("/domains")
    public ResponseEntity<List<DomainDTO>> getDomains(){
        List<DomainDTO> domains = mainService.getDomains();
        return ResponseEntity.ok(domains);
    }

    @PostMapping("/jobs")
    public ResponseEntity<Map<String, String>> createJob(@RequestBody JobDTO[] jobDTOs){
        Map<String, String> filesAndDomains = mainService.createJob(jobDTOs);
        return ResponseEntity.ok(filesAndDomains);
    }

    @GetMapping("/jobs")
    public ResponseEntity<List<JobDTO>> getAllJobs(){
        List<JobDTO> jobs = mainService.getAllJobs();
        return ResponseEntity.ok(jobs);
    }

    @GetMapping("/jobs/priority")
    public ResponseEntity<String> getPriority(@RequestParam("fileName") String fileName){
        Priority priority = mainService.getPriority(fileName);
        return ResponseEntity.ok(priority.toString());
    }

}

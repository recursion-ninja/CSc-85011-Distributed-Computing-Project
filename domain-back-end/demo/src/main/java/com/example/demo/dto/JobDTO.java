package com.example.demo.dto;

import com.example.demo.domain.Job;
import com.example.demo.domain.Priority;
import com.example.demo.domain.Status;

public class JobDTO {
    private Long id;

    private Long size;

    private Integer numOfMinutes;

    private String userEmail;

    private String fileName;

    private Priority priority;

    private Boolean pivot;

    private Status status;

    public JobDTO() {
    }

    public JobDTO(Job job) {
        this.id = job.getId();
        this.fileName = job.getFileName();
        this.numOfMinutes = job.getNumOfMinutes();
        this.pivot = job.getPivot();
        this.priority = job.getPriority();
        this.size = job.getSize();
        this.status = job.getStatus();
        this.userEmail = job.getUserEmail();
    }



    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getNumOfMinutes() {
        return numOfMinutes;
    }

    public void setNumOfMinutes(Integer numOfMinutes) {
        this.numOfMinutes = numOfMinutes;
    }

    public String getUserEmail() {
        return userEmail;
    }

    public void setUserEmail(String userEmail) {
        this.userEmail = userEmail;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String scriptName) {
        this.fileName = fileName;
    }

    public Priority getPriority() {
        return priority;
    }

    public void setPriority(Priority priority) {
        this.priority = priority;
    }

    public Boolean getPivot() {
        return pivot;
    }

    public void setPivot(Boolean pivot) {
        this.pivot = pivot;
    }
}

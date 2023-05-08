package com.example.demo.dto;

import com.example.demo.domain.Job;

public class JobDTO {
    private Long id;

    private Integer numOfCPUCores;

    private Integer numOfGPUCores;

    private Integer memorySize;

    private Integer diskSpace;

    private Integer numOfMinutes;

    private String userEmail;

    private String fileName;


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getNumOfCPUCores() {
        return numOfCPUCores;
    }

    public void setNumOfCPUCores(Integer numOfCPUCores) {
        this.numOfCPUCores = numOfCPUCores;
    }

    public Integer getNumOfGPUCores() {
        return numOfGPUCores;
    }

    public void setNumOfGPUCores(Integer numOfGPUCores) {
        this.numOfGPUCores = numOfGPUCores;
    }

    public Integer getMemorySize() {
        return memorySize;
    }

    public void setMemorySize(Integer memorySize) {
        this.memorySize = memorySize;
    }

    public Integer getDiskSpace() {
        return diskSpace;
    }

    public void setDiskSpace(Integer diskSpace) {
        this.diskSpace = diskSpace;
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
}

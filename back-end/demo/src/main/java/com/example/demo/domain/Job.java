package com.example.demo.domain;

import javax.persistence.*;

@Entity
@Table(name="job")
public class Job {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "num_of_cpu_cores")
    private Integer numOfCPUCores;

    @Column(name = "num_of_gpu_cores")
    private Integer numOfGPUCores;

    @Column(name = "memory_size")
    private Integer memorySize;

    @Column(name = "disk_space")
    private Integer diskSpace;

    @Column(name = "num_of_minutes")
    private Integer numOfMinutes;

    @Column(name = "user_email")
    private String userEmail;

    @ManyToOne
    private Domain domain;

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

    public Domain getDomain() {
        return domain;
    }

    public void setDomain(Domain domain) {
        this.domain = domain;
    }
}

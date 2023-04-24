package com.example.demo.domain;

import javax.persistence.*;
import java.util.List;

@Entity
@Table(name = "domain")
public class Domain {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "num_of_available_cpu_cores")
    private Integer numOfAvailableCPUCores;

    @Column(name = "num_of_available_gpu_cores")
    private Integer numOfAvailableGPUCores;

    @Column(name = "available_memory_size")
    private Integer availableMemorySize;

    @Column(name = "available_disk_space")
    private Integer availableDiskSpace;

    @OneToMany(mappedBy = "domain")
    private List<Job> jobs;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getNumOfAvailableCPUCores() {
        return numOfAvailableCPUCores;
    }

    public void setNumOfAvailableCPUCores(Integer numOfAvailableCPUCores) {
        this.numOfAvailableCPUCores = numOfAvailableCPUCores;
    }

    public Integer getNumOfAvailableGPUCores() {
        return numOfAvailableGPUCores;
    }

    public void setNumOfAvailableGPUCores(Integer numOfAvailableGPUCores) {
        this.numOfAvailableGPUCores = numOfAvailableGPUCores;
    }

    public Integer getAvailableMemorySize() {
        return availableMemorySize;
    }

    public void setAvailableMemorySize(Integer availableMemorySize) {
        this.availableMemorySize = availableMemorySize;
    }

    public Integer getAvailableDiskSpace() {
        return availableDiskSpace;
    }

    public void setAvailableDiskSpace(Integer availableDiskSpace) {
        this.availableDiskSpace = availableDiskSpace;
    }

    public List<Job> getJobs() {
        return jobs;
    }

    public void setJobs(List<Job> jobs) {
        this.jobs = jobs;
    }
}

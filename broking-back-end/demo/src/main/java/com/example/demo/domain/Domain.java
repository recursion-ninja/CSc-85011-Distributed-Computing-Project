package com.example.demo.domain;

import javax.persistence.*;
import java.util.List;

@Entity
@Table(name = "domains")
public class Domain {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "ip")
    private String ip;

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

    public String getIp() {
        return ip;
    }

    public void setIp(String ip) {
        this.ip = ip;
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

package com.example.demo.domain;

import javax.persistence.*;

@Entity
@Table(name="jobs")
public class Job {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "size")
    private Long size;

    @Column(name = "num_of_minutes")
    private Integer numOfMinutes;

    @Column(name = "user_email")
    private String userEmail;

    @Column(name = "file_name")
    private String fileName;

    @Column(name = "status")
    @Enumerated(EnumType.STRING)
    private Status status;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getSize() {
        return size;
    }

    public void setSize(Long size) {
        this.size = size;
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

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }
}

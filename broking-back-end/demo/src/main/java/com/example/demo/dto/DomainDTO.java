package com.example.demo.dto;

import com.example.demo.domain.Domain;

public class DomainDTO {

    private Long id;

    private String ip;

    private Integer availableDiskSpace;

    public DomainDTO() {
    }

    public DomainDTO(Domain domain) {
        this.id = domain.getId();
        this.ip = domain.getIp();
        this.availableDiskSpace = domain.getAvailableDiskSpace();
    }

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

}

# CSc-85011-Distributed-Computing-Project
Course project for CSc 85011 - Distributed Computing of Spring 2023

## Front-end Terminal User Interface (TUI)

This project contains a TUI for a user to connect to the Science Brokering Service gateway and submit jobs for processing.
The TUI allows for specifying job priority as well as a soft time requirement for completion.

### Front-end TUI installation

To install the TUI from source, run the following:

```
make install
```

Subsequently, the front-end TUI binary will be available at `./bin/science-broker-terminal` located from the project root.

### Back-end 

To run the the backend, execute the following commands: 

For the broker:
```
java -jar ./broker-backend.jar
```

For the domains:
```
java -jar ./domains-backend.jar
```

Both executables are available in the `./bin` folder. They can also be generated from the source code with maven:
```
cd broking-back-end/demo/
mvn clean install
```
```
cd domain-back-end/demo/
mvn clean install
```
The generated executables will be in `/broking-back-end/demo/target` and `domain-back-end/demo/target` folders.

The domain executable should be executed on three different servers. Once all three domains are up and running, sent the IP addresses of the domains to the broker:

```
curl --location --request POST 'http://<broker-ip>:8080/api/domains' --header 'Content-Type: application/json' \
--data-raw '{
    "ip": "<domain-ip>",
    "availableDiskSpace": 12000
```

## Example usage of front-end TUI

[![asciicast](https://asciinema.org/a/Sd9Yiks5qpvJQM2zw4KA5RREN.svg)](https://asciinema.org/a/Sd9Yiks5qpvJQM2zw4KA5RREN?speed=2&theme=solarized-dark&autoplay=1)


## Video corpora used

[Robust Distributed Decision-Making in Robot Swarms/kilobot experiment videos/2 sites/three_valued](https://data.bris.ac.uk/data/dataset/441f63ca99a1cc7cd7338d08e94d67a3)

[Robust Distributed Decision-Making in Robot Swarms/kilobot experiment videos/2 sites/three_valued_robustness](https://data.bris.ac.uk/data/dataset/de10260e4fe410aadb5983dde5650ecc)

[Temple et al 2020 Octopus pol vision/TempleEtAl_2020_OctopusPolVision](https://data.bris.ac.uk/data/dataset/ce7aa7b7bf9ac967fec2cc7ec21a61d2)

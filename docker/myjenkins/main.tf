/*
  Manages a local docker image with a Jenkins instance in it, for OHRRPGCE stuff
*/

terraform {
  required_providers {
    docker = {
      source = "terraform-providers/docker"
    }
  }
}

provider "docker" {}

resource "docker_image" "jenkins" {
  # Local image based on jenkins/jenkins:lts but with stuff added for docker-in-docker
  name         = "myjenkins"
  keep_locally = false
}

resource "docker_container" "jenkins" {
  image = docker_image.jenkins.latest
  name  = "jenkins"
  ports {
    internal = 8080
    external = 8080
  }
  ports {
    internal = 50000
    external = 50000
  }
  volumes {
    volume_name     = docker_volume.jenkins_home.name
    container_path  = "/var/jenkins_home"
  }
  volumes {
    host_path = "/var/run/docker.sock"
    container_path  = "/var/run/docker.sock"
  }
}

resource "docker_volume" "jenkins_home" {
  name = "jenkins_home_vol"
}

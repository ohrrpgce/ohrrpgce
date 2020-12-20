## Jenkins image for OHRRPGCE

For running a local Jenkins instance.

This assumes you have installed docker and terraform

This is a Jenkins docker image that is capable of running other docker
images as build agents. (They actually run as sibling containers, even
though they behave as nested child containers)

A teraform main.tf file is included for starting up the container with
the correct volume mounts (You could definitely accomplish the same
thing with docker-compose)

## Instructions

  # Build the docker image
  docker build -t myjenkins ./
  
  # Start the docker image
  terraform init
  terraform apply

## Backup

You'll definitely want to back up your jenkins_home_vol docker volume
because it contains all the Jenkins state and configuration, including
plugins and jobs and logs. The quick and dirty way to back it up is
to back up /var/lib/docker/volumes/jenkins_home_vol/_data
(that exact location might vary depending on what platform you are
running docker on)

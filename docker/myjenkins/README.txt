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

## First Run

On the first run, you will be prompted to unlock Jenkins
with the initial admin password from:
`/var/jenkins_home/secrets/initialAdminPassword`
but if that path is inside the docker container,
so if you don't want to connect a shell to the container
you can look instead on the host at:
`/var/lib/docker/volumes/jenkins_home_vol/_data/secrets/initialAdminPassword`

After that you will be prompted to install the default recommended
plugins, and to create your real admin user password

## Extra Plugins

* Go to "Manage Jenkins"
* Go to "Manage Plugins"
* From "Available" search for "Docker Pipeline" and install it
* Allow Jenkins to restart

## Setting up OHRRPGCE build job

From the main Jenkins page, pick "New Item"

Name it "OHRRPGCE" and pick "Multibranch Pipeline"

Fill in the following:
* Display Name: OHRRPGCE
* Add source:
  * pick "git" (use plain "git" not "github" even if the url is github)
  * Input the repository url https://github.com/ohrrpgce/ohrrpgce.git
    (If using your private fork you might need to add auth)
  * In "Behaviors" add "filter by name" with wildcards and include "wip"
* Skip all other options and pick "Add" at the bottom
* Wait for the "Scan multibranch pipeline" to find the Jenkinsfile
  on the wip branch
* OHRRPGCE wip job will be added, and should attempt to build itself
* The first build will take a long time because the docker images have
  to be built

## Backup

You'll definitely want to back up your jenkins_home_vol docker volume
because it contains all the Jenkins state and configuration, including
plugins and jobs and logs. The quick and dirty way to back it up is
to back up /var/lib/docker/volumes/jenkins_home_vol/_data
(that exact location might vary depending on what platform you are
running docker on)

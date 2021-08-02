def default_UPLOAD_USER = "james_paige"
def default_UPLOAD_HOST = "motherhamster.org"
def default_UPLOAD_FOLDER = "HamsterRepublic.com/ohrrpgce/nightly-test/"
def default_SSH_CREDS = "hamsterrepublic-ohrrpgce"

pipeline {
    agent any
    triggers {
        pollSCM 'H/5 * * * *'
    }
    options {
	    quietPeriod(60)
    }
    parameters {
        string(
            name: "UPLOAD_USER",
            defaultValue: params.UPLOAD_USER ?: default_UPLOAD_USER,
            description: "This username will be used when doing the ssh upload of build artifacts"
        )
        string(
            name: "UPLOAD_HOST",
            defaultValue: params.UPLOAD_HOST ?: default_UPLOAD_HOST,
            description: "This hostname will be the destination for doing the ssh upload of build artifacts"
        )
        string(
            name: "UPLOAD_FOLDER",
            defaultValue: params.UPLOAD_FOLDER ?: default_UPLOAD_FOLDER,
            description: "This is the destination folder on the remote host for doing the ssh upload of build artifacts"
        )
        string(
            name: "SSH_CREDS",
            defaultValue: params.SSH_CREDS ?: default_SSH_CREDS,
            description: "This is the ID you used when you added your ssh private key to Jenkins' credential vault. The matching public key must be in .ssh/authorized_keys for the upload host"
        )
    }
    stages {
        stage('docker-images') {
            steps {
                sh 'docker build --tag bobthehamster/freebasic ./docker/freebasic/'
                sh 'docker build --tag bobthehamster/ohrrpgce-build-env ./docker/ohrrpgce-build-env/'
                sh 'docker build --tag bobthehamster/ohrrpgce-build-env-x86 ./docker/ohrrpgce-build-env/'
            }
        }
        stage('cleanup-distrib') {
            steps {
                sh 'rm -f distrib/*'
            }
        }
        stage('build-linux-x86_64') {
            agent { docker { image 'bobthehamster/ohrrpgce-build-env' } }
            environment {
                OHR_SKIP_X86 = "yes"
            }
            steps {
                sh './distrib-linux.sh'
                sh 'ls -l distrib/'
                stash name: 'distrib_dir_linux_x86_64', includes: 'distrib/*'
            }
        }
        stage('build-linux-x86') {
            agent { docker { image 'bobthehamster/ohrrpgce-build-env-x86' } }
            environment {
                OHR_SKIP_X86_64 = "yes"
            }
            steps {
                sh './distrib-linux.sh'
                sh 'ls -l distrib/'
                stash name: 'distrib_dir_linux_x86', includes: 'distrib/*'
            }
        }
        stage('upload-ohrrpgce') {
            environment {
                USER = "${params.UPLOAD_USER}"
                HOST = "${params.UPLOAD_HOST}"
                FOLDER = "${params.UPLOAD_FOLDER}"
            }
            steps {
                unstash 'distrib_dir_linux_x86_64'
                unstash 'distrib_dir_linux_x86'
                sh 'ls -l distrib/'
                withCredentials([sshUserPrivateKey(credentialsId: params.SSH_CREDS, keyFileVariable: 'SSH_KEYFILE')]) {
                    sh '''
                      scp -i $SSH_KEYFILE -o StrictHostKeyChecking=no \
                        distrib/ohrrpgce-linux-*-wip-x86_64.tar.bz2 \
                        $USER@$HOST:$FOLDER/ohrrpgce-linux-$BRANCH_NAME-x86_64.tar.bz2
                      scp -i $SSH_KEYFILE -o StrictHostKeyChecking=no \
                        distrib/ohrrpgce-player-linux-bin-minimal-*-wip-x86_64.zip \
                        $USER@$HOST:$FOLDER/ohrrpgce-player-linux-bin-minimal-$BRANCH_NAME-x86_64.zip
                      scp -i $SSH_KEYFILE -o StrictHostKeyChecking=no \
                        distrib/ohrrpgce-linux-*-wip-x86.tar.bz2 \
                        $USER@$HOST:$FOLDER/ohrrpgce-linux-$BRANCH_NAME-x86.tar.bz2
                      scp -i $SSH_KEYFILE -o StrictHostKeyChecking=no \
                        distrib/ohrrpgce-player-linux-bin-minimal-*-wip-x86.zip \
                        $USER@$HOST:$FOLDER/ohrrpgce-player-linux-bin-minimal-$BRANCH_NAME-x86.zip
                      if [ "$BRANCH_NAME" = "wip" ] ; then
                        ssh -i $SSH_KEYFILE -o StrictHostKeyChecking=no \
                          -- ln -s ohrrpgce-player-linux-bin-minimal-$BRANCH_NAME-x86.zip $FOLDER/ohrrpgce-player-linux-bin-minimal.zip
                      fi
                    '''
                }
            }
        }
    }
}

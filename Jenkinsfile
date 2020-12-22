def default_UPLOAD_USER = "james_paige"
def default_UPLOAD_HOST = "motherhamster.org"
def default_UPLOAD_FOLDER = "HamsterRepublic.com/ohrrpgce/nightly-test/"

pipeline {
    agent any
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
    }
    stages {
        stage('docker-image-freebasic') {
            steps {
                sh 'docker build --tag bobthehamster/freebasic ./docker/freebasic/'
            }
        }
        stage('docker-image-ohrrpgce') {
            steps {
                sh 'docker build --tag bobthehamster/ohrrpgce-build-env ./docker/ohrrpgce-build-env/'
            }
        }
        stage('cleanup-distrib') {
            steps {
                sh 'rm -f distrib/*'
            }
        }
        stage('build-ohrrpgce') {
            agent { docker { image 'bobthehamster/ohrrpgce-build-env' } }
            environment {
                OHR_SKIP_X86 = "yes"
            }
            steps {
                sh './distrib-linux.sh'
                sh 'ls -l distrib/'
                stash name: 'distrib_dir', includes: 'distrib/*'
            }
        }
        stage('upload-ohrrpgce') {
            environment {
                UPLOAD_USER = "${params.UPLOAD_USER}"
                UPLOAD_HOST = "${params.UPLOAD_HOST}"
                UPLOAD_FOLDER = "${params.UPLOAD_FOLDER}"
            }
            steps {
                unstash 'distrib_dir'
                sh 'ls -l distrib/'
                withCredentials([sshUserPrivateKey(credentialsId: 'hamsterrepublic-ohrrpgce', keyFileVariable: 'SSH_KEYFILE')]) {
                    sh '''
                      scp -i $SSH_KEYFILE -o StrictHostKeyChecking=no \
                        distrib/ohrrpgce-linux-*-wip-x86_64.tar.bz2 \
                        $UPLOAD_USER@$UPLOAD_HOST:$UPLOAD_FOLDER/ohrrpgce-linux-$BRANCH_NAME-x86_64.tar.bz2
                      scp -i $SSH_KEYFILE -o StrictHostKeyChecking=no \
                        distrib/ohrrpgce-player-linux-bin-minimal-*-wip-x86_64.zip \
                        $UPLOAD_USER@$UPLOAD_HOST:$UPLOAD_FOLDER/ohrrpgce-player-linux-bin-minimal-$BRANCH_NAME-x86_64.zip
                    '''
                }
            }
        }
    }
}

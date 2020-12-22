pipeline {
    agent any
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
                UPLOAD_USER = 'james_paige'
                UPLOAD_HOST = 'motherhamster.org'
                UPLOAD_FOLDER = 'HamsterRepublic.com/ohrrpgce/nightly-test/'
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

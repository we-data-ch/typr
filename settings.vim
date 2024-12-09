command! -bar DeployHelper !bash deploy.sh
command! Deploy Build | DeployHelper

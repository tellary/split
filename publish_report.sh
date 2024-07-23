set -x
set -u

REPORT=$1
BUCKET=split.apps.tellary.ru

aws s3api put-bucket-website --bucket $BUCKET --website-configuration file://website.json
ls -1 $REPORT.jsexe | xargs -I{} -n 1 -P 10 aws s3 cp $REPORT.jsexe/{} s3://$BUCKET/$REPORT/{} --metadata-directive REPLACE --cache-control max-age=60
ls -1 $REPORT.jsexe | xargs -I{} -n 1 -P 10 aws s3api put-object-acl --bucket $BUCKET --key $REPORT/{} --acl public-read


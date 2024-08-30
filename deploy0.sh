set -x
set -u

nix-build -A ghcjs

BUCKET=split.apps.tellary.ru
FILES='index.html runmain.js all.min.js'

aws s3api put-bucket-website --bucket $BUCKET --website-configuration file://website.json

pushd result

echo -n $FILES | xargs -d ' ' -I{} -n 1 -P 10 aws s3 cp split.jsexe/{} s3://$BUCKET/{} --metadata-directive REPLACE --cache-control max-age=60
echo -n $FILES | xargs -d ' ' -I{} -n 1 -P 10 aws s3api put-object-acl --bucket $BUCKET --key {} --acl public-read

popd

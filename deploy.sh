set -x
set -u

nix-build -A ghcjs

BUCKET=split.apps.tellary.ru

pushd result

aws s3api put-bucket-website --bucket $BUCKET --website-configuration file://website.json
ls -1 split.jsexe | xargs -I{} -n 1 -P 10 aws s3 cp split.jsexe/{} s3://$BUCKET/{} --metadata-directive REPLACE --cache-control max-age=60
ls -1 split.jsexe | xargs -I{} -n 1 -P 10 aws s3api put-object-acl --bucket $BUCKET --key {} --acl public-read

popd

set -x
set -u
set -e

# Example: bash deploy0.sh ghcjs2024coimbra 2024coimbra sample-data/
TARGET="${1:-ghcjs}"
EXEC="${2:-split}"
DEST="${3:-}"

nix-build -A $TARGET

BUCKET=split.apps.tellary.ru
FILES='index.html runmain.js all.min.js'

aws s3api put-bucket-website --bucket $BUCKET --website-configuration file://website.json

pushd result

echo -n $FILES | xargs -d ' ' -I{} -n 1 -P 10 aws s3 cp $EXEC.jsexe/{} s3://$BUCKET/$DEST{} --metadata-directive REPLACE --cache-control max-age=60
echo -n $FILES | xargs -d ' ' -I{} -n 1 -P 10 aws s3api put-object-acl --bucket $BUCKET --key $DEST{} --acl public-read

popd

#!/usr/bin/env bash
set -euo pipefail
usage() {
    printf '%s\n' 'Usage: scripts/publish-public.sh [--apply] [--target PATH]' '' 'Creates an allow-listed public candidate from this private repository.' 'Default: dry run. --apply copies locally only; it never commits or pushes.'
}
apply=0
target=''
while [ "$#" -gt 0 ]; do
    case "$1" in
        --apply) apply=1 ;;
        --target)
            [ "$#" -ge 2 ] || { echo '--target requires a path' >&2; exit 2; }
            target=$2
            shift
            ;;
        --help|-h) usage; exit 0 ;;
        *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
    esac
    shift
done
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
private_repo=$(cd "$script_dir/.." && pwd)
target=${target:-"$(dirname "$private_repo")/LinkEDA-public"}
[ -d "$target/.git" ] || { echo "Public candidate is not a Git repository: $target" >&2; exit 1; }
[ -z "$(git -C "$private_repo" status --porcelain)" ] || { echo 'Private repository has uncommitted changes.' >&2; exit 1; }
[ -z "$(git -C "$target" status --porcelain)" ] || { echo 'Public candidate has uncommitted changes.' >&2; exit 1; }
stage_dir=$(mktemp -d /private/tmp/linkeda-publish.XXXXXX)
check_dir=$(mktemp -d /private/tmp/linkeda-public-check.XXXXXX)
cleanup() { rm -rf "$stage_dir" "$check_dir"; }
trap cleanup EXIT
copy_file() {
    [ -f "$private_repo/$1" ] && cp "$private_repo/$1" "$stage_dir/$1"
}
copy_dir() {
    [ -d "$private_repo/$1" ] || return 0
    rsync -a --exclude '.git/' --exclude '.DS_Store' --exclude '*.Rcheck/' --exclude '*.tar.gz' --exclude '*.tgz' --exclude '*.o' --exclude '*.so' --exclude '*.dylib' --exclude '*.dll' --exclude '*.exe' --exclude 'Rplots.pdf' "$private_repo/$1/" "$stage_dir/$1/"
}
for file in DESCRIPTION NAMESPACE LICENSE README.md .Rbuildignore .gitignore configure cleanup; do
    copy_file "$file"
done
for directory in R src man inst tests examples bench; do
    copy_dir "$directory"
done
copy_dir website
forbidden=$(find "$stage_dir" -type f \( -name '.RData' -o -name '.Rhistory' -o -name '.Ruserdata' -o -name '.Renviron' -o -name '*.Rcheck' -o -name 'Rplots.pdf' -o -name '*.tar.gz' -o -name '*.tgz' -o -name '*.o' -o -name '*.so' -o -name '*.dylib' -o -name '*.dll' -o -name '*.exe' \) -print -quit)
[ -z "$forbidden" ] || { echo "Forbidden artefact found: $forbidden" >&2; exit 1; }
cp -R "$stage_dir/." "$check_dir/"
(
    cd "$check_dir"
    R CMD build .
    R CMD check --no-manual LinkEDA_*.tar.gz
)
echo 'Prospective public diff:'
diff -ru --exclude '.git' --exclude 'PUBLICATION_STATUS.md' "$target" "$stage_dir" || diff_status=$?
case ${diff_status:-0} in
    0|1) ;;
    *) exit "$diff_status" ;;
esac
if [ "$apply" -eq 0 ]; then
    echo 'Dry run complete. Re-run with --apply to copy the reviewed candidate locally.'
    exit 0
fi
rsync -a "$stage_dir/" "$target/"
echo 'Copied to the local public candidate. No commit or push was performed.'

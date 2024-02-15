#/usr/bin/bash
awk -f - README.org > init.el <<'EOF'
{
    if (tolower($0) ~ /^#\+begin_src elisp\s*$/) {
        flag=1;
    } else if (tolower($0) ~ /^#\+end_src/) {
        flag=0;
        print "";
    } else if (flag) {
        print $0;
    }
}
EOF

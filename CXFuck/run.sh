if [ $# != 1 ]
then
    echo "usame $0 <file>"
    exit 1
else
    echo "Code:"
    gcc cxfuck.c -o com
    ./com $1
    gcc -o runner runner.s

    rm com
    rm runner.s

    echo ""
    echo "Program:"
    ./runner
fi
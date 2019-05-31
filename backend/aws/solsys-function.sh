function handler () {
    EVENT_DATA="$1"
    NUMBER=`echo "$EVENT_DATA" | jq .number`
    #cd ../../../solsys-functional/backend/
    ./run-main "$NUMBER" >&2
}

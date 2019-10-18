fun day(num:int*int*int)=
    365*(#1 num)+31*(#2 num)+(#3 num);

(*Q1*)
fun is_older(a:int*int*int,b:int*int*int)=
    day(a)<day(b)

(*Q2*)
fun number_in_month(date:(int*int*int) list, month:int)=

    if null date
    then 0
    else
        let val num = number_in_month(tl date, month)
        in
        if #2 (hd date) = month
        then num+1
        else num
        end

(*Q3*)
fun number_in_months(date:(int*int*int) list, month:int list)=
    if null month
    then 0
    else if number_in_month(date, hd month)=0
    then number_in_months(date, tl month)
    else number_in_months(date,tl month) + number_in_month(date, hd month)

(*Q4*)
fun dates_in_month(date:(int*int*int) list, month:int)=
    if null date
    then []
    else
        let val tl_dates = dates_in_month(tl date, month)
        in
        if #2 (hd date) = month
        then (hd date)::tl_dates
        else tl_dates
        end

(*Q5*)
fun dates_in_months(date:(int*int*int) list, month:int list)=
    if null month
    then []
    else if dates_in_month(date, hd month)=[]
    then dates_in_months(date, tl month)
    else dates_in_month(date, hd month)@dates_in_months(date, tl month)

(*Q6*)
fun get_nth(slist:string list, n: int)=
    if n=1
    then hd slist
    else
        get_nth(tl slist, n-1)

(*Q7*)
fun date_to_string(date:int*int*int)=
    let val year = Int.toString(#1 date)
        val day = Int.toString(#3 date)^","
        val mlist = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val month = get_nth(mlist, #2 date)
    in
        month^" "^day^" "^year
    end

(*Q8*)
fun number_before_reaching_sum(sum: int, plist: int list)=
    if sum<=(hd plist)
    then 0
    else
        let val n=number_before_reaching_sum(sum-(hd plist), tl plist)
        in
            if sum > (hd plist)
            then n+1
            else n
        end

(*Q9*)
fun what_month(day:int)=
    let val ilist=[31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, ilist)+1
    end

(*Q10*)
fun month_range(day1, day2)=
    if day1>day2
    then []
    else if day1<day2
    then what_month(day1)::month_range(day1+1,day2)
    else [what_month(day1)]

(*Q11*)
fun oldest(date:(int*int*int) list)=
    if null date
    then NONE
    else
        let val tl_old = oldest(tl date)
        in
            if not (isSome (oldest(tl date))) orelse is_older(hd date, valOf tl_old)
            then SOME (hd date)
            else tl_old
        end

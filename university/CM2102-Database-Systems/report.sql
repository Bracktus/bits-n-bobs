clear screen;
accept Report_File char prompt 'Enter a file name for summary report';
accept month char prompt 'Enter a month in the format JAN,FEB etc..';

col name format A20 heading 'Name';
col del# format 99999 heading 'Delivery|No';
col item# format 99999 heading 'Item No';
col description format A20 heading 'Description';
col quantity format 99999 heading 'Quantity';
col price format 9999.99 'Item|Price';
col (itm.price*dlved.quantity) format 9999.99 heading 'Cost';

set Pause off;
set Feedback off;
set Space 6;
set newpage 2;
set pagesize 54;
set linesize 500;
set underline =;

break on name skip 1 on del# skip 2;
compute sum of (itm.price*dlved.quantity) on name

spool &Report_File;

select cust.name, del.del#, itm.item#, itm.description, dlved.quantity, itm.price, (itm.price*dlved.quantity) 
from customer cust, delivery del, item itm, deliveredQty dlved, standingOrder std
where (dlved.Del# = del.Del#) and (std.order# = dlved.order#) and (std.item# = itm.item#)
and del.delDate like '%&month%'
order by cust.name, del.delDate;

spool off;

clear breaks;
clear columns;
clear computes;
set Pause on;

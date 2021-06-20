
spool pop;

insert into customer
(Cust#, Name, Address)
values(1, 'BBC Wales', 'Llantrisaant Rad, Cardiff, CF5 2YQ');

insert into customer
(Cust#, Name, Address)
values(2, 'Pyramid', 'Cardiff Road, Cardiff, CF5 1AB');

insert into customer
(Cust#, Name, Address)
values(3, 'Sony Broadcast', 'London Road, Reading, RD1 2CD');

insert into customer
(Cust#, Name, Address)
values(4, 'Thompson Broadcast', 'Reading Road, Slough, SL2 3EF');

insert into customer
(Cust#, Name, Address)
values(5, 'Radio Spares', 'London Road, Birmingham, BH3 4GH');

insert into item
(Item#, Description, Price, Weight)
values(1, 'CD', 1.50, 0.10);

insert into item
(Item#, Description, Price, Weight)
values(2, 'Disks', 1.00, 0.15);

insert into item
(Item#, Description, Price, Weight)
values(3, 'Cartridges', 2.00, 0.35);

insert into item 
(Item#, Description, Price, Weight)
values(4, 'Paper', 3.00, 3.15);

insert into item 
(Item#, Description, Price, Weight)
values(5, 'Toner', 27.50, 5.75);

insert into item 
(Item#, Description, Price, Weight)
values(6, 'Pen', 0.25, 0.01);

insert into Standingorder
(Order#, StartDate, EndDate, Cust#, Item#)
values(1, TO_DATE('01-01-20', 'DD-MM-YYYY'), TO_DATE('31-01-20', 'DD-MM-YYYY'), 1, 1);

insert into Standingorder
(Order#, StartDate, EndDate, Cust#, Item#)
values(2, TO_DATE('01-01-20', 'DD-MM-YYYY'), TO_DATE('31-01-20', 'DD-MM-YYYY'), 2, 2);

insert into Standingorder
(Order#, StartDate, EndDate, Cust#, Item#)
values(3, TO_DATE('01-01-20', 'DD-MM-YYYY'), TO_DATE('31-01-20', 'DD-MM-YYYY'), 3, 3);

insert into Standingorder
(Order#, StartDate, EndDate, Cust#, Item#)
values(4, TO_DATE('01-01-20', 'DD-MM-YYYY'), TO_DATE('31-01-20', 'DD-MM-YYYY'), 4, 4);

insert into Standingorder
(Order#, StartDate, EndDate, Cust#, Item#)
values(5, TO_DATE('01-01-20', 'DD-MM-YYYY'), TO_DATE('31-01-20', 'DD-MM-YYYY'), 5, 5);

insert into Standingorder
(Order#, StartDate, EndDate, Cust#, Item#)
values(6, TO_DATE('01-01-20', 'DD-MM-YYYY'), TO_DATE('31-01-20', 'DD-MM-YYYY'), 5, 6);

insert into Invoice
(Inv#, InvDate, Total, Cust#)
values(1, TO_DATE('01-01-20', 'DD-MM-YYYY'), 7.50, 1);

insert into Invoice
(Inv#, InvDate, Total, Cust#)
values(2, TO_DATE('01-01-20', 'DD-MM-YYYY'), 10.00, 2);

insert into Invoice
(Inv#, InvDate, Total, Cust#)
values(3, TO_DATE('01-01-20', 'DD-MM-YYYY'), 30.00, 3);

insert into Invoice
(Inv#, InvDate, Total, Cust#)
values(4, TO_DATE('01-01-20', 'DD-MM-YYYY'), 60.00, 4);

insert into Invoice
(Inv#, InvDate, Total, Cust#)
values(5, TO_DATE('01-01-20', 'DD-MM-YYYY'), 695.00, 5);

insert into Delivery
(Del#, DelDate, Inv#)
values(1, TO_DATE('01-01-20', 'DD-MM-YYYY'), 1);

insert into Delivery
(Del#, DelDate, Inv#)
values(2, TO_DATE('01-01-20', 'DD-MM-YYYY'), 2);

insert into Delivery
(Del#, DelDate, Inv#)
values(3, TO_DATE('01-01-20', 'DD-MM-YYYY'), 3);

insert into Delivery
(Del#, DelDate, Inv#)
values(4, TO_DATE('01-01-20', 'DD-MM-YYYY'), 4);

insert into Delivery
(Del#, DelDate, Inv#)
values(5, TO_DATE('01-01-20', 'DD-MM-YYYY'), 5);

insert into Delivery
(Del#, DelDate, Inv#)
values(7, TO_DATE('07-01-20', 'DD-MM-YYYY'), 1);

insert into Delivery
(Del#, DelDate, Inv#)
values(8, TO_DATE('07-01-20', 'DD-MM-YYYY'), 2);

insert into Delivery
(Del#, DelDate, Inv#)
values(9, TO_DATE('07-01-20', 'DD-MM-YYYY'), 3);

insert into Delivery
(Del#, DelDate, Inv#)
values(10,TO_DATE( '07-01-20', 'DD-MM-YYYY'), 4);

insert into Delivery
(Del#, DelDate, Inv#)
values(11,TO_DATE( '07-01-20', 'DD-MM-YYYY'), 5);

insert into Delivery
(Del#, DelDate, Inv#)
values(13,TO_DATE( '14-01-20', 'DD-MM-YYYY'), 1);

insert into Delivery
(Del#, DelDate, Inv#)
values(14,TO_DATE( '14-01-20', 'DD-MM-YYYY'), 2);

insert into Delivery
(Del#, DelDate, Inv#)
values(15,TO_DATE( '14-01-20', 'DD-MM-YYYY'), 3);

insert into Delivery
(Del#, DelDate, Inv#)
values(16,TO_DATE( '14-01-20', 'DD-MM-YYYY'), 4);

insert into Delivery
(Del#, DelDate, Inv#)
values(17,TO_DATE( '14-01-20', 'DD-MM-YYYY'), 5);

insert into Delivery
(Del#, DelDate, Inv#)
values(19,TO_DATE( '21-01-20', 'DD-MM-YYYY'), 1);

insert into Delivery
(Del#, DelDate, Inv#)
values(20,TO_DATE( '21-01-20', 'DD-MM-YYYY'), 2);

insert into Delivery
(Del#, DelDate, Inv#)
values(21,TO_DATE( '21-01-20', 'DD-MM-YYYY'), 3);

insert into Delivery
(Del#, DelDate, Inv#)
values(22,TO_DATE( '21-01-20', 'DD-MM-YYYY'), 4);

insert into Delivery
(Del#, DelDate, Inv#)
values(23,TO_DATE( '21-01-20', 'DD-MM-YYYY'), 5);

insert into Delivery
(Del#, DelDate, Inv#)
values(25,TO_DATE( '28-01-20', 'DD-MM-YYYY'), 1);

insert into Delivery
(Del#, DelDate, Inv#)
values(26,TO_DATE( '28-01-20', 'DD-MM-YYYY'), 2);

insert into Delivery
(Del#, DelDate, Inv#)
values(27,TO_DATE( '28-01-20', 'DD-MM-YYYY'), 3);

insert into Delivery
(Del#, DelDate, Inv#)
values(28,TO_DATE( '28-01-20', 'DD-MM-YYYY'), 4);

insert into Delivery
(Del#, DelDate, Inv#)
values(29,TO_DATE( '28-01-20', 'DD-MM-YYYY'), 5);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(1,1,1);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(2,2,2);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(3,3,3);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(4,4,4);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(5,5,5);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(6,5,6);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(1,7,1);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(2,8,2);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(3,9,3);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(4,10,4);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(5,11,5);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(6,11,6);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(1,13,1);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(2,14,2);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(3,15,3);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(4,16,4);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(5,17,5);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(6,17,6);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(1,19,1);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(2,20,2);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(3,21,3);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(4,22,4);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(5,23,5);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(6,23,6);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(1,25,1);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(2,26,2);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(3,27,3);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(4,28,4);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(5,29,5);

insert into DeliveredQty
(Order#, Del#, Quantity)
values(6,29,6);

spool off;

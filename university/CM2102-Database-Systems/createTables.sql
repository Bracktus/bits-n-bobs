spool createTables;

create table Invoice
(
	Inv#       number(5,0)    primary key,
	InvDate    date	          not null,
	Total      number(5,2)    not null,
	Cust#      number(5,0)    not null,
	Constraint Invoice_Cust#_FK
	 	       foreign key(Cust#)
		       references Customer(Cust#)
)

create table Customer
(
	Cust#   number(5,0) primary key,
	Name    varchar2(32),
	Address varchar2(64)
)

create table covers
(
    Pay# number(5,0),
    Inv# number(5,0),
	Amount number(5,2) not null,
    Constraint Covers_Pay#_FK
	foreign key(Pay#)
    references Payment(Pay#),
    Constraint Covers_Inv#_FK
    foreign key(Inv#)
    references Invoice(Inv#),
    Constraint Covers_CK
    primary key(Pay#, Inv#)
)
/
create table Item
(
    Item#           number(5,0) primary key,
    Description     varchar2(50),
    Price           number(5,2) not null,
    Weight          number(4,2)
) 
/
create table delivery
(
	Del# number(5,0) primary key,
	DelDate date not null,
	Inv# number(5,0),
	Constraint Invoice_Inv#_FK
	foreign key(Inv#)
	references Invoice(Inv#)
)

create table DeliveredQty
(
	Order#	number(5,0),
	Del# 	number(5,0),
	Quantity number(4,0) not null,
	constraint DeliveryQty_Del#_FK
		   foreign key(Del#)
		   references Delivery(Del#),
	constraint DeliveryQty_CK
		   Unique(Order#, Del#)
)

create table StandingOrder
(
	Order# 	    number(5,0) primary key,
	StartDate   date not null,
	EndDate     date not null,
	Cust# 	    number(5,0),
	Item# 	    number(5,0),
    Constraint  StandingOrder_Cust#_FK
                foreign key(Cust#)
                references Customer(Cust#),
    Constraint  StandingOrder_Item#_FK
                foreign key(Item#)
                references Item(Item#)
)
create table payment
(
	Pay# number(5,0) primary key,
	PayDate date not null,
	Cust# number(5,0) not null,
	Constraint Delivery_Cust#_FK
	foreign key(Cust#)
	references Customer(Cust#)
)

spool off;

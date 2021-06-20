
select C.name 
from customer C, standingOrder S, item I
where (I.Description = 'CD' or I.Description = 'Disks')
and (S.Item# = I.item#)
and (S.Cust# = C.Cust#);

select count(order#)
from standingorder S, item I
where (I.description = 'Cartridges' or I.description = 'Paper') 
and (S.item# = I.item#);

select description, count(I.item#)
from item I, standingOrder S
where (S.item# = I.item#)
group by description;

select inv# 
from delivery D
group by inv#
having count(inv#) > 1;

select I.inv#
from invoice I, covers C
where I.inv# = C.inv#
group by I.inv#, I.total
having sum(C.amount) < I.total
    

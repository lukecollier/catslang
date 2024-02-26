select * from qt_linear_attribution_without_window(
        (select * from ( 
            select
                rampid as rampid,
                date as exposure_timestamp ,
                campaign_id AS cut_value 
            from
                tiktok_exposures
            where
                date >= '2023-12-01'
            and
                date <= '2023-12-30'
    )),
    (select * 
    from (
        select
            distinct(a2.rampid) as rampid,
            order_date as conversion_timestamp,
            1 as conversion_value
        from 
            transactionsV2 a1
        inner join 
            lrid_luid_mappings a2
        on 
            a1.user_id=a2.user_id
        where 
            order_date >= '2023-12-01'
        and 
            order_date <= '2023-12-30'
    )),
    (select * 
     from (
            select
                distinct(rampid) as rampid,
                segment
                from (
                select distinct(a2.rampid) as rampid,
                    'all_audiences' as segment
                from
                    transactionsV2 a1
                inner join
                    lrid_luid_mappings a2
                on
                    a1.user_id=a2.user_id
                where
                    a1.order_date >= '2023-12-01'
                and
                    a1.order_date <= '2023-12-30'
        union
            select
                distinct(rampid) as rampid,
                'all_audiences' as segment
            from 
                tiktok_exposures
            where
                date >= '2023-12-01'
            and
                date <= '2023-12-30'))),
        '',
        'campaign_id',
        14,
        '2023-12-01',   
        '2023-12-30');

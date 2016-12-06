INSERT INTO genre (genre_title) VALUES ('裏原系');
INSERT INTO genre (genre_title) VALUES ('渋谷系');
INSERT INTO genre (genre_title) VALUES ('オレオレ系');

INSERT INTO tag (tag_title) VALUES ('Tシャツ');
INSERT INTO tag (tag_title) VALUES ('ジャケット');
INSERT INTO tag (tag_title) VALUES ('ジーンズ');

INSERT INTO look   (look_create_dt, look_update_dt, look_title, look_description, look_genre_id)
       VALUES (NOW(),NOW(),'コーデ1タイトル','コーデ1説明',1);
       
INSERT INTO look   (look_create_dt, look_update_dt, look_title, look_description, look_genre_id)
       VALUES (NOW(),NOW(),'コーデ2タイトル','コーデ2説明',2);

INSERT INTO look   (look_create_dt, look_update_dt, look_title, look_description, look_genre_id)
       VALUES (NOW(),NOW(),'コーデ3タイトル','コーデ3説明',1);

INSERT INTO look   (look_create_dt, look_update_dt, look_title, look_description, look_genre_id)
       VALUES (NOW(),NOW(),'コーデ4タイトル','コーデ4説明',3);
       
INSERT INTO look   (look_create_dt, look_update_dt, look_title, look_description, look_genre_id)
       VALUES (NOW(),NOW(),'コーデ5タイトル','コーデ5説明',1);


INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (1,1);
INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (1,2);
INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (1,3);
INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (2,1);
INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (2,2);
INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (3,1);
INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (3,3);
INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (4,3);



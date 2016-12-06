DROP TABLE look_has_tag;
DROP TABLE look;
DROP TABLE tag;
DROP TABLE genre;


CREATE TABLE genre (
        genre_id INT NOT NULL AUTO_INCREMENT
       ,genre_title nvarchar(31) NOT NULL
       ,PRIMARY KEY (genre_id)
);


CREATE TABLE tag (
        tag_id INT NOT NULL AUTO_INCREMENT
       ,tag_title nvarchar(31) NOT NULL
       ,PRIMARY KEY (tag_id)
);


CREATE TABLE look (
        look_id INT NOT NULL AUTO_INCREMENT
       ,look_create_dt DATETIME NOT NULL
       ,look_update_dt DATETIME NOT NULL
       ,look_title nvarchar(31) NOT NULL
       ,look_description nvarchar(400) NOT NULL
       ,look_genre_id INT NOT NULL
       ,PRIMARY KEY (look_id)
       ,FOREIGN KEY (look_genre_id) REFERENCES genre (genre_id)
);

CREATE TABLE look_has_tag (
        lht_look_id INT NOT NULL
       ,lht_tag_id INT NOT NULL
       ,PRIMARY KEY (lht_look_id,lht_tag_id)
       ,FOREIGN KEY (lht_look_id) REFERENCES look (look_id)
       ,FOREIGN KEY (lht_tag_id) REFERENCES tag (tag_id)
);


SELECT * FROM genre;
SELECT * FROM tag;
SELECT * FROM look;
SELECT * FROM look_has_tag;


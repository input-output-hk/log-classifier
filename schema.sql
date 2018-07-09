-- PRAGMA foreign_keys = "1";

CREATE TABLE `ticket_info` (
	`tiId`	INTEGER,
	`tiRequesterId`	INTEGER NOT NULL,
	`tiAssigneeId`	INTEGER,
	`tiUrl`	TEXT NOT NULL,
	`tiTags`	TEXT NOT NULL,
	`tiStatus`	TEXT NOT NULL,
	PRIMARY KEY(tiId)
) WITHOUT ROWID;

CREATE TABLE `ticket_comment` (
	`id`	INTEGER,
	`ticket_id`	INTEGER NOT NULL,
	`body`	TEXT NOT NULL,
	`is_public`	INTEGER NOT NULL,
	`author_id`	INTEGER NOT NULL,
	PRIMARY KEY(id),
	FOREIGN KEY(`ticket_id`) REFERENCES ticket_info(tId)
) WITHOUT ROWID;

CREATE TABLE `comment_attachment` (
	`aId`	INTEGER,
	`comment_id`	INTEGER NOT NULL,
	`aURL`	TEXT NOT NULL,
	`aContentType`	TEXT NOT NULL,
	`aSize`	INTEGER NOT NULL,
	PRIMARY KEY(aId),
	FOREIGN KEY(`comment_id`) REFERENCES ticket_comment ( ticket_id )
) WITHOUT ROWID;

CREATE TABLE `attachment_content` (
	`attachment_id`	INTEGER,
	`content`	BLOB NOT NULL,
	PRIMARY KEY(attachment_id),
	FOREIGN KEY(`attachment_id`) REFERENCES comment_attachment(aId)
);

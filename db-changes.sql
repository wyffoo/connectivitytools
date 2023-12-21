ALTER TABLE giga.projects MODIFY COLUMN mode enum('schools','countries','global') CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT 'schools' NOT NULL;
ALTER TABLE giga.users MODIFY COLUMN app_mode enum('schools','countries','global') CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT 'schools' NOT NULL;

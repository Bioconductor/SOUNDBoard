--
-- BOARD_TABLE
--

CREATE TABLE IF NOT EXISTS board (
    -- internal
    create_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    modify_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    -- user-provided
    board_uid TEXT NOT NULL UNIQUE,
    description TEXT
);

-- BOARD_MODIFY
UPDATE board
    SET modify_time_ = CURRENT_TIMESTAMP
    WHERE board_uid = :board_uid;

--
-- CASES_TABLE
--

CREATE TABLE IF NOT EXISTS cases (
    -- internal
    create_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    modify_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    -- user-provided
    board_uid TEXT,
    case_uid TEXT NOT NULL UNIQUE,
    age INTEGER CHECK (age > 0),
    sex TEXT CHECK (sex = "Male" or sex = "Female"),
    FOREIGN KEY(board_uid) REFERENCES board(board_uid)
);

-- CASES_DELETE
DELETE FROM cases WHERE case_uid = :case_uid;

-- CASES_MODIFY
UPDATE cases
    SET modify_time_ = CURRENT_TIMESTAMP
    WHERE case_uid = :case_uid;

--
-- ASSAY_TABLE
--

CREATE TABLE IF NOT EXISTS assay (
    -- internal
    assay_uid_ INTEGER PRIMARY KEY AUTOINCREMENT,
    create_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    modify_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    -- user-provided
    case_uid TEXT,
    assay TEXT,
    description TEXT,
    resource TEXT,
    FOREIGN KEY(case_uid) REFERENCES cases(case_uid)
);

-- ASSAY_MODIFY
UPDATE assay
    SET modify_time_ = CURRENT_TIMESTAMP
    WHERE assay_uid = :assay_uid;

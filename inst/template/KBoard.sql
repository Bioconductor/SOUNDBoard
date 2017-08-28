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
    sex TEXT CHECK (sex = "male" or sex = "female"),
    primary_site TEXT,
    diagnosis TEXT,
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

--
-- COPY_NUMBER_TABLE
--

CREATE TABLE IF NOT EXISTS copy_number (
    -- internal
    create_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    modify_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    -- user-provided
    case_uid TEXT,
    assay TEXT NOT NULL UNIQUE,
    snv_total INTEGER CHECK (snv_total > 0),
    cnv_total INTEGER CHECK (cnv_total > 0),
    gene TEXT,
    mutation_type TEXT,
    copy_number TEXT,
    FOREIGN KEY(assay) REFERENCES assay(assay)
);

-- COPY_NUMBER_MODIFY
UPDATE copy_number
    SET modify_time_ = CURRENT_TIMESTAMP
    WHERE assay = :assay:

--
-- MUTATIONS_TABLE
--

CREATE TABLE IF NOT EXISTS mutation (
    -- internal
    create_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    modify_time_ DATETIME DEFAULT CURRENT_TIMESTAMP,
    -- user-provided
    case_uid TEXT,
    assay TEXT NOT NULL UNIQUE,
    assay_uid TEXT,
    gene TEXT,
    mutation_type TEXT,
    variant_fq FLOAT,
    FOREIGN KEY(assay) REFERENCES assay(assay)
)

-- MUTATION_MODIFY
UPDATE mutation
    SET modify_time_ = CURRENT_TIMESTAMP
    WHERE assay = :assay:

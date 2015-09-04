--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: accounts; Type: TABLE; Schema: public; Owner: housetab_user; Tablespace: 
--

CREATE TABLE accounts (
    id integer NOT NULL,
    name text NOT NULL,
    email text NOT NULL,
    password bytea NOT NULL,
    salt bytea NOT NULL,
    tutorial_active boolean NOT NULL,
    record_history boolean NOT NULL
);


ALTER TABLE accounts OWNER TO housetab_user;

--
-- Name: accounts_id_seq; Type: SEQUENCE; Schema: public; Owner: housetab_user
--

CREATE SEQUENCE accounts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE accounts_id_seq OWNER TO housetab_user;

--
-- Name: accounts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: housetab_user
--

ALTER SEQUENCE accounts_id_seq OWNED BY accounts.id;


--
-- Name: entries; Type: TABLE; Schema: public; Owner: housetab_user; Tablespace: 
--

CREATE TABLE entries (
    id integer NOT NULL,
    account_id integer NOT NULL,
    who integer NOT NULL,
    what text NOT NULL,
    category text NOT NULL,
    date timestamp with time zone NOT NULL,
    howmuch double precision NOT NULL,
    whopays integer[] NOT NULL
);


ALTER TABLE entries OWNER TO housetab_user;

--
-- Name: entries_id_seq; Type: SEQUENCE; Schema: public; Owner: housetab_user
--

CREATE SEQUENCE entries_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE entries_id_seq OWNER TO housetab_user;

--
-- Name: entries_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: housetab_user
--

ALTER SEQUENCE entries_id_seq OWNED BY entries.id;


--
-- Name: log; Type: TABLE; Schema: public; Owner: housetab_user; Tablespace: 
--

CREATE TABLE log (
    id integer NOT NULL,
    account_id integer NOT NULL,
    type text NOT NULL,
    who_old integer,
    who_new integer,
    what_old text,
    what_new text,
    category_old text,
    category_new text,
    date_old timestamp with time zone,
    date_new timestamp with time zone,
    howmuch_old double precision,
    howmuch_new double precision,
    whopays_old integer[],
    whopays_new integer[]
);


ALTER TABLE log OWNER TO housetab_user;

--
-- Name: log_id_seq; Type: SEQUENCE; Schema: public; Owner: housetab_user
--

CREATE SEQUENCE log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE log_id_seq OWNER TO housetab_user;

--
-- Name: log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: housetab_user
--

ALTER SEQUENCE log_id_seq OWNED BY log.id;


--
-- Name: migrations; Type: TABLE; Schema: public; Owner: housetab_user; Tablespace: 
--

CREATE TABLE migrations (
    name text NOT NULL,
    run_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE migrations OWNER TO housetab_user;

--
-- Name: persons; Type: TABLE; Schema: public; Owner: housetab_user; Tablespace: 
--

CREATE TABLE persons (
    id integer NOT NULL,
    account_id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE persons OWNER TO housetab_user;

--
-- Name: persons_id_seq; Type: SEQUENCE; Schema: public; Owner: housetab_user
--

CREATE SEQUENCE persons_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE persons_id_seq OWNER TO housetab_user;

--
-- Name: persons_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: housetab_user
--

ALTER SEQUENCE persons_id_seq OWNED BY persons.id;


--
-- Name: shares; Type: TABLE; Schema: public; Owner: housetab_user; Tablespace: 
--

CREATE TABLE shares (
    id integer NOT NULL,
    person_id integer NOT NULL,
    start timestamp with time zone NOT NULL,
    value double precision NOT NULL
);


ALTER TABLE shares OWNER TO housetab_user;

--
-- Name: shares_id_seq; Type: SEQUENCE; Schema: public; Owner: housetab_user
--

CREATE SEQUENCE shares_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE shares_id_seq OWNER TO housetab_user;

--
-- Name: shares_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: housetab_user
--

ALTER SEQUENCE shares_id_seq OWNED BY shares.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY accounts ALTER COLUMN id SET DEFAULT nextval('accounts_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY entries ALTER COLUMN id SET DEFAULT nextval('entries_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY log ALTER COLUMN id SET DEFAULT nextval('log_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY persons ALTER COLUMN id SET DEFAULT nextval('persons_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY shares ALTER COLUMN id SET DEFAULT nextval('shares_id_seq'::regclass);


--
-- Name: accounts_name_key; Type: CONSTRAINT; Schema: public; Owner: housetab_user; Tablespace: 
--

ALTER TABLE ONLY accounts
    ADD CONSTRAINT accounts_name_key UNIQUE (name);


--
-- Name: accounts_pkey; Type: CONSTRAINT; Schema: public; Owner: housetab_user; Tablespace: 
--

ALTER TABLE ONLY accounts
    ADD CONSTRAINT accounts_pkey PRIMARY KEY (id);


--
-- Name: entries_pkey; Type: CONSTRAINT; Schema: public; Owner: housetab_user; Tablespace: 
--

ALTER TABLE ONLY entries
    ADD CONSTRAINT entries_pkey PRIMARY KEY (id);


--
-- Name: log_pkey; Type: CONSTRAINT; Schema: public; Owner: housetab_user; Tablespace: 
--

ALTER TABLE ONLY log
    ADD CONSTRAINT log_pkey PRIMARY KEY (id);


--
-- Name: migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: housetab_user; Tablespace: 
--

ALTER TABLE ONLY migrations
    ADD CONSTRAINT migrations_pkey PRIMARY KEY (name);


--
-- Name: persons_pkey; Type: CONSTRAINT; Schema: public; Owner: housetab_user; Tablespace: 
--

ALTER TABLE ONLY persons
    ADD CONSTRAINT persons_pkey PRIMARY KEY (id);


--
-- Name: shares_pkey; Type: CONSTRAINT; Schema: public; Owner: housetab_user; Tablespace: 
--

ALTER TABLE ONLY shares
    ADD CONSTRAINT shares_pkey PRIMARY KEY (id);


--
-- Name: entries_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY entries
    ADD CONSTRAINT entries_account_id_fkey FOREIGN KEY (account_id) REFERENCES accounts(id);


--
-- Name: entries_who_fkey; Type: FK CONSTRAINT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY entries
    ADD CONSTRAINT entries_who_fkey FOREIGN KEY (who) REFERENCES persons(id);


--
-- Name: log_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY log
    ADD CONSTRAINT log_account_id_fkey FOREIGN KEY (account_id) REFERENCES accounts(id);


--
-- Name: log_who_new_fkey; Type: FK CONSTRAINT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY log
    ADD CONSTRAINT log_who_new_fkey FOREIGN KEY (who_new) REFERENCES persons(id);


--
-- Name: log_who_old_fkey; Type: FK CONSTRAINT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY log
    ADD CONSTRAINT log_who_old_fkey FOREIGN KEY (who_old) REFERENCES persons(id);


--
-- Name: persons_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY persons
    ADD CONSTRAINT persons_account_id_fkey FOREIGN KEY (account_id) REFERENCES accounts(id);


--
-- Name: shares_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: housetab_user
--

ALTER TABLE ONLY shares
    ADD CONSTRAINT shares_person_id_fkey FOREIGN KEY (person_id) REFERENCES persons(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--


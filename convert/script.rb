# coding: utf-8
require 'mongo'
require 'pg'

M = Mongo::Client.new([ '127.0.0.1:27017' ], :database => 'housetab')
PG = PG::Connection.open(:dbname => 'housetab_devel', :user => "housetab_user", :password => "111", :host => "localhost")


def get_account_id(htid)

  id = BSON::ObjectId.from_string(htid)

  account = M[:users].find(:_id => id).first

  if not account.nil?
    res = PG.exec_params("SELECT id from accounts where name = $1",
                         [account["accountName"]])
    if not res.num_tuples.zero? then
      res.first.first[1].to_i
    else
      nil
    end
  else
    nil
  end

end


def get_person_id(pid)

  person = M[:people].find(:_id => BSON::ObjectId.from_string(pid)).first

  if person
    account_id = get_account_id(person["htid"])
    if account_id
      res = PG.exec_params("SELECT id from persons where name = $1 and account_id = $2",
                           [person["name"], account_id])
      if not res.num_tuples.zero? then
        res.first.first[1].to_i
      else
        nil
      end
    else
      nil
    end
  else
    nil
  end

end

def get_person_id!(pid)
  id = get_person_id(pid)
  if id.nil?
    raise ArgumentError
  end
  id
end

def convert_date!(date)
  DateTime.new(date["year"], date["month"], date["day"])
end

M[:users].find().each do |account|
  res = PG.exec_params("SELECT id from accounts where email = $1", [account["accountEmail"]])

  if res.num_tuples.zero?

    # NOTE(dbp 2015-04-26): So this is hilarious. I'm not sure who is to blame, but
    # the haskell application for housetab with mongo was able to store raw binary data
    # in a string type. I don't see a way to get that out via the ruby client, as it
    # seems to be interpreted as a UTF-8 string, which it's not. But I can still do some
    # juggling in haskell to get this out, so at this stage, we'll shell out to the getpass
    # project to get the bytes of the password.
    puts "Shelling out to get password for #{account['accountName']}..."
    pass = `cabal run -v0 "#{account['accountName']}"`
    pass = pass.gsub!(/[\[\]]/,'').split(/\s*,\s*/).map(&:to_i).pack("C*")

    PG.exec_params('INSERT INTO accounts (name, email, password, salt, tutorial_active, record_history) values ($1,$2,$3::bytea,$4,$5,$6)',
                   [account["accountName"], account["accountEmail"],
                    PG.escape_bytea(pass),
                    PG.escape_bytea(account["salt"]),
                    account["tutorialActive"],
                    account["recordHistory"]])
  else
    # already exists
  end


  #  {"_id"=><BSON::ObjectId:0x38129880 data=50a9839f3575051d9f010228>,
  # "accountName"=>"___",
  # "accountEmail"=>"___",
  # "accountEmailChange"=>nil,
  # "currentResult"=>
  #  {"people"=>
  #    [[{"_id"=><BSON::ObjectId:0x38127060 data=50a984063575051d9f010235>,
  #       "htid"=><BSON::ObjectId:0x38126680 data=50a9839f3575051d9f010228>,
  #       "name"=>"___",
  #       "shares"=>
  #        [{"date"=>{"year"=>2012, "month"=>11, "day"=>17}, "value"=>50.0}]},
  #      519.74,
  #      72.40500000000009],
  #     [{"_id"=><BSON::ObjectId:0x38146380 data=50a9862c3575051d9f010259>,
  #       "htid"=><BSON::ObjectId:0x38146160 data=50a9839f3575051d9f010228>,
  #       "name"=>"___",
  #       "shares"=>
  #        [{"date"=>{"year"=>2012, "month"=>11, "day"=>17}, "value"=>50.0}]},
  #      664.5500000000001,
  #      -72.40499999999997]],
  #   "currentdate"=>{"year"=>2013, "month"=>6, "day"=>9}},
  # "accountReset"=>nil,
  # "accountActivate"=>nil,
  # "tutorialActive"=>false,
  # "recordHistory"=>true,
  # "created_at"=>2012-11-19 00:55:59 UTC,
  # "email"=>nil,
  # "password"=>  "___",
  # "salt"=>"981627bf8aff16da5eb5a9141a535ce0767517e3b1775952aa61cf0f7d13321e",
  # "activated_at"=>nil,
  # "suspended_at"=>nil,
  # "persistence_token"=>nil,
  # "current_login_at"=>2014-08-31 23:11:24 UTC,
  # "last_login_at"=>2014-08-31 23:11:14 UTC,
  # "current_login_ip"=>"127.0.0.1",
  # "last_login_ip"=>"127.0.0.1",
  # "login_count"=>31,
  # "failed_login_count"=>2,
  # "updated_at"=>2014-08-31 23:11:24 UTC}]

end

M[:people].find().each do |person|

  account_id = get_account_id(person["htid"])

  if account_id
    if get_person_id(person["_id"])
    # already exists
    else
      res = PG.exec_params('INSERT INTO persons (account_id, name) values ($1,$2) RETURNING id',
                           [account_id, person["name"]])
      if not res.num_tuples.zero? then
        person_id =  res.first.first[1].to_i
        person["shares"].each do |share|
          date = convert_date!(share['date'])
          PG.exec_params('INSERT INTO shares (person_id, start, value) values ($1,$2,$3)',
                         [person_id, date, share["value"]])
        end
      else
        puts "Couldn't get id from person: #{person}"
      end
    end
  else
    puts "No account for person: #{person['name']} #{person['htid']}"
  end



  # { "_id" : ObjectId("4e0fb5c43575052150000001"),
  # "htid" : ObjectId("4e0fb5c33575052150000000"),
  # "name" : "_",
  # "shares" :
  # [ { "date" :
  #      { "year" : 2009, "month" : 12, "day" : 1 },
  #     "value" : 15 },
  # ] }


end



M[:entries].find().each do |entry|

  account_id = get_account_id(entry["htid"])

  if account_id
    begin
      who_id = get_person_id(entry["who"])

      if who_id
        date = convert_date!(entry["when"])

        res = PG.exec_params('INSERT INTO entries (account_id, who, what, category, date, howmuch) values ($1,$2,$3,$4,$5,$6) RETURNING id',
                             [account_id, who_id, entry["what"],
                              entry["category"], date, entry["howmuch"]])

        if not res.num_tuples.zero? then
          entry_id =  res.first.first[1].to_i
          whopays = entry["whopays"].map{|w| get_person_id(w)}
          whopays.each do |person_id|
            PG.exec_params('INSERT INTO entries_whopays (entry_id, person_id) values ($1,$2)',
                           [entry_id, person_id])
          end
        else
          puts "Couldn't get ID from entry: #{entry}"
        end
      else
        puts "Couldn't find who person: #{entry}"
      end
    rescue ArgumentError
      puts "Couldn't figure out date: #{entry}"
    end
  else
    puts "No account for entry: #{entry['_id']} #{entry['htid']}"
  end


  #  {"_id"=><BSON::ObjectId:0x38127660 data=4e0fb5c43575052150000068>,
  # "htid"=><BSON::ObjectId:0x38127360 data=4e0fb5c33575052150000000>,
  # "who"=><BSON::ObjectId:0x38126980 data=4e0fb5c43575052150000005>,
  # "what"=>"food",
  # "category"=>"misc",
  # "when"=>{"year"=>2010, "month"=>2, "day"=>20},
  # "howmuch"=>4.0,
  # "whopays"=>
  #  [<BSON::ObjectId:0x38147200 data=4e0fb5c43575052150000001>,
  #   <BSON::ObjectId:0x38146780 data=4e0fb5c43575052150000004>,
  #   <BSON::ObjectId:0x38146560 data=4e0fb5c43575052150000005>,
  #   <BSON::ObjectId:0x38146220 data=4e0fb5c43575052150000003>,
  #   <BSON::ObjectId:0x38145700 data=4e0fb5c43575052150000002>]},


end

M[:history].find().each do |log|

  account_id = get_account_id(log["htid"])

  if account_id
    begin

      type = log['type']

      if type == 'edit'
        who_old = get_person_id!(log['who']['o'])
        who_new = if !log['who']['n'].nil? then get_person_id!(log['who']['n']) else nil end
        date_old = convert_date!(log['when']['o'])
        date_new = if !log['when']['n'].nil? then convert_date!(log['when']['n']) else nil end
        category_old = log['category']['o']
        category_new = log['category']['n']
        what_old = log['what']['o']
        what_new = log['what']['n']
        howmuch_old = log['howmuch']['o']
        howmuch_new = log['howmuch']['n']
      elsif type == 'add' || type == 'delete'
        who_old = nil
        who_new = get_person_id!(log['who'])
        date_old = nil
        date_new = convert_date!(log['when'])
        category_old = nil
        category_new = log['category']
        howmuch_old = nil
        howmuch_new = log['howmuch']
      else
        raise "Don't understand type: #{type} for log entry #{log}"
      end


      res = PG.exec_params('INSERT INTO log (account_id, type, who_old, who_new, category_old, category_new, what_old, what_new, date_old, date_new, howmuch_old, howmuch_new) values ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12) RETURNING id',
                           [account_id, type, who_old,
                            who_new, category_old, category_new,
                            what_old, what_new,
                            date_old, date_new,
                            howmuch_old, howmuch_new])

      if not res.num_tuples.zero? then
        log_id =  res.first.first[1].to_i

        if type == 'edit'
          log["whopays"]['o'].map{|w| get_person_id(w)}.each do |person_id|
            PG.exec_params('INSERT INTO log_whopays_old (log_id, person_id) values ($1,$2)',
                           [log_id, person_id])
          end
          if !log["whopays"]['n'].nil?
            log["whopays"]['n'].map{|w| get_person_id(w)}.each do |person_id|
              PG.exec_params('INSERT INTO log_whopays_new (log_id, person_id) values ($1,$2)',
                             [log_id, person_id])
            end
          end
        elsif type == 'add' || type == 'delete'
          if !log["whopays"].nil?
            log["whopays"].map{|w| get_person_id(w)}.each do |person_id|
              PG.exec_params('INSERT INTO log_whopays_new (log_id, person_id) values ($1,$2)',
                             [log_id, person_id])
            end
          end
        else
          raise "Don't understand type: #{type} for log entry #{log}"
        end
      else
        puts "Couldn't get ID from inserted log: #{log}"
      end

    rescue
      puts "Couldn't figure out log entry: #{log}"
    end
  else
    puts "No account for log: #{log}"
  end


  #  {"_id"=><BSON::ObjectId:0x54900460 data=4e0fbdb0357505210a000151>,
  # "htid"=><BSON::ObjectId:0x54900160 data=4e0fb5c735750521500003b6>,
  # "type"=>"edit",
  # "date"=>2011-07-03 00:54:08 UTC,
  # "who"=>
  #  {"o"=><BSON::ObjectId:0x54898840 data=4e0fb5c735750521500003b8>, "n"=>nil},
  # "what"=>{"o"=>"__", "n"=>nil},
  # "category"=>{"o"=>"misc", "n"=>"household"},
  # "when"=>{"o"=>{"year"=>2011, "month"=>6, "day"=>10}, "n"=>nil},
  # "howmuch"=>{"o"=>12.0, "n"=>nil},
  # "whopays"=>
  #  {"o"=>
  #    [<BSON::ObjectId:0x54885460 data=4e0fb5c735750521500003b7>,
  #     <BSON::ObjectId:0x54885300 data=4e0fb5c735750521500003b8>],
  #   "n"=>nil}},



end

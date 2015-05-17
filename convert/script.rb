# coding: utf-8
require 'mongo'
require 'pg'

M = Mongo::Client.new([ '127.0.0.1:27017' ], :database => 'housetab')
P = PG::Connection.open(:dbname => 'housetab_devel', :user => "housetab_user", :password => "111", :host => "localhost")

Mongo::Logger.logger.level = Logger::WARN


def get_account_id(htid)

  id = BSON::ObjectId.from_string(htid)

  account = M[:users].find(:_id => id).first

  if not account.nil?
    res = P.exec_params("SELECT id from accounts where name = $1",
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
      res = P.exec_params("SELECT id from persons where name = $1 and account_id = $2",
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
  if date["month"] > 12 && date["day"] <= 12
    # NOTE(dbp 2015-05-17): One entry has this problem.
    month = date["day"]
    day = date["month"]
  else
    month = date["month"]
    day = date["day"]
  end
  DateTime.new(date["year"], month, day)
end

def convert_date_safe(date)
  begin
    convert_date!(date)
  rescue ArgumentError
    nil
  end
end

M[:users].find().each do |account|
  res = P.exec_params("SELECT id from accounts where name = $1", [account["accountName"]])

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

    P.exec_params('INSERT INTO accounts (name, email, password, salt, tutorial_active, record_history) values ($1,$2,$3::bytea,$4,$5,$6)',
                   [account["accountName"], account["accountEmail"],
                    P.escape_bytea(pass),
                    P.escape_bytea(account["salt"]),
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
      res = P.exec_params('INSERT INTO persons (account_id, name) values ($1,$2) RETURNING id',
                           [account_id, person["name"]])
      if not res.num_tuples.zero? then
        person_id =  res.first.first[1].to_i
        person["shares"].each do |share|
          date = convert_date!(share['date'])
          P.exec_params('INSERT INTO shares (person_id, start, value) values ($1,$2,$3)',
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
        whopays = entry["whopays"].map{|w| get_person_id(w)}.to_a

        res = P.exec_params('INSERT INTO entries (account_id, who, what, category, date, howmuch, whopays) values ($1,$2,$3,$4,$5,$6,$7) RETURNING id',
                             [account_id, who_id, entry["what"],
                              entry["category"], date, entry["howmuch"], "{" + whopays.join(",") + "}"])
      else
        puts "Couldn't find who person: #{entry}"
      end
    rescue ArgumentError => e
      puts e
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

  def format_array(a)
    if a
      "{" + a.join(",") + "}"
    else
      nil
    end
  end


  if account_id
    begin

      type = log['type']

      if type == 'edit'
        who_old = get_person_id!(log['who']['o'])
        who_new = if !log['who']['n'].nil? then get_person_id!(log['who']['n']) else nil end
        date_old = convert_date_safe(log['when']['o'])
        date_new = if !log['when']['n'].nil? then convert_date_safe(log['when']['n']) else nil end
        category_old = log['category']['o']
        category_new = log['category']['n']
        what_old = log['what']['o']
        what_new = log['what']['n']
        howmuch_old = log['howmuch']['o']
        howmuch_new = log['howmuch']['n']
        if log["whopays"]['o']
          whopays_old = format_array(log["whopays"]['o'].map{|w| get_person_id(w)}.to_a)
        else
          whopays_old = nil
        end
        if log["whopays"]['n']
          whopays_new = format_array(log["whopays"]['n'].map{|w| get_person_id(w)}.to_a)
        else
          whopays_new = nil
        end
      elsif type == 'delete'
        who_new = nil
        who_old = get_person_id!(log['who'])
        date_new = nil
        date_old = convert_date_safe(log['when'])
        category_new = nil
        category_old = log['category']
        howmuch_new = nil
        howmuch_old = log['howmuch']
        what_new = nil
        what_old = log['what']
        whopays_old = format_array(log["whopays"].map{|w| get_person_id(w)}.to_a)
        whopays_new = nil
      elsif type == 'add'
        who_old = nil
        who_new = get_person_id!(log['who'])
        date_old = nil
        date_new = convert_date_safe(log['when'])
        category_old = nil
        category_new = log['category']
        howmuch_old = nil
        howmuch_new = log['howmuch']
        what_old = nil
        what_new = log['what']
        whopays_old = format_array(log["whopays"].map{|w| get_person_id(w)}.to_a)
        whopays_new = nil
      else
        raise "Don't understand type: #{type} for log entry #{log}"
      end


      res = P.exec_params('INSERT INTO log (account_id, type, who_old, who_new, category_old, category_new, what_old, what_new, date_old, date_new, howmuch_old, howmuch_new, whopays_old, whopays_new) values ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14) RETURNING id',
                           [account_id, type, who_old,
                            who_new, category_old, category_new,
                            what_old, what_new,
                            date_old, date_new,
                            howmuch_old, howmuch_new,
                            whopays_old, whopays_new])
    rescue StandardError => e
      puts e
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

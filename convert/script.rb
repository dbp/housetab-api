# coding: utf-8
require 'mongo'
require 'pg'

m = Mongo::Client.new([ '127.0.0.1:27017' ], :database => 'housetab')
pg = PG::Connection.open(:dbname => 'housetab_devel', :user => "housetab_user", :password => "111")


def get_account_id(m, pg, htid)

  id = BSON::ObjectId.from_string(htid)

  account = m[:users].find(:_id => id).first

  if not account.nil?
    res = pg.exec_params("SELECT id from accounts where email = $1",
                         [account["accountEmail"]])
    if not res.num_tuples.zero? then
      res.first.first[1].to_i
    else
      nil
    end
  else
    nil
  end

end


def get_person_id(m, pg, pid)

  person = m[:people].find(:_id => BSON::ObjectId.from_string(pid)).first

  if person
    account_id = get_account_id(m, pg, person["htid"])
    if account_id
      res = pg.exec_params("SELECT id from persons where name = $1 and account_id = $2",
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


m[:users].find().each do |account|
  next

  res = pg.exec_params("SELECT id from accounts where email = $1", [account["accountEmail"]])

  if res.num_tuples.zero?
    pg.exec_params('INSERT INTO accounts (name, email) values ($1,$2)',
                   [account["accountName"], account["accountEmail"]])
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

m[:people].find().each do |person|
  next

  account_id = get_account_id(m, pg, person["htid"])

  if account_id
    if get_person_id(m, pg, person["_id"])
      # already exists
    else
      pg.exec_params('INSERT INTO persons (account_id, name) values ($1,$2)',
                     [account_id, person["name"]])
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



m[:entries].find().each do |entry|

  account_id = get_account_id(m, pg, entry["htid"])

  if account_id
    begin
      who_id = get_person_id(m, pg, entry["who"])

      date = DateTime.new(entry["when"]["year"], entry["when"]["month"], entry["when"]["day"])

      pg.exec_params('INSERT INTO entries (account_id, who, what, category, date, howmuch) values ($1,$2,$3,$4,$5,$6)',
                     [account_id, who_id, entry["what"],
                      entry["category"], date, entry["howmuch"]])
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

m[:history].find().each do |log|

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

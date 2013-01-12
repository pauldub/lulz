module SmoothCloud
	class File
		def initialize(options = {})
			@errors = options[:errors]
		end

		def self.create(attributes = {})
			err, response = post "/users", headers
		rescue Exception => e#Errors::BadAttribute => e
			self.new errors: e#.errors
		end

		def headers
			headers = {
				:mon_header => '1234'
			}
			SmoothCloud.headers.merge headers
		end
	end

	class << self
		def configure(&block)
			yield configuration
		end

		def configuration
			@coniguration ||= {}
		end

		def headers
			{
				:access_token => oauth.token(configuration[:access_id])
			}
		end
	end
end

class HTTP
	include Logging
	extend Logging::StdOut

	def initialize(url)
	end

	[:get, :post, :put, :del],each do |verb|
		def_method verb do |*args|
			url, body = args
			body ||= {}

			request = Net::HTTP::Request {
			 	:method => verb,
				:url => URI.parse(url),
				:body => body
			}

			JSON.parse(request.body)
		rescue HTTP::Timeout => e
			log e.message, :error
		end
	end
end

HTTP.methode

http = HTPP.new
http.log

module Logging
	def included(base)
		base.send :extend, ClassMethods
	end

	class ClassMethods
		def log(msg, level = :debug)
			msg = msg.inspect unless msg.is_a? String

			puts "#{level}: #{msg}"
		end
	end
end



HTTP.post 'http://google.com', :q => "les chats"

SmoothCloud.configure do |config|
	config[:access_id] = '12346'
	config[:access_secret] = '12346'
end

user = SmoothCloud::User.create name: 'paul', password: '123456'

errors = {
	:name => ['must start with a Capital letter',
						'must not suck']
}

if user.errors.any?
	"#{2+2}"
	000001

	"%06d" % 10
	000010

	dirs = `ls`
	"test" =~ /^\d$/

	title = `mon-article.json`[/^([\w+-])\.json$/, 1]

	error.each do |field, messages|
		puts "#{field}: #{messages * ','}."
	end
else
	puts "Success, user has been created."
end

require 'rubygems'
require_gem 'rake'
require_gem 'snepo_generator'

require 'rake/tasklib'
require 'rake/clean'
require 'snepo_generator/snepo_task'

module Snepo
  class DistributorTask
    include Snepo

    # version to tag depot release as.
    attr_accessor :tag
    attr_accessor :components
    
    attr_accessor :deploy_host
    attr_accessor :svn_proto
    attr_accessor :svn_port
    attr_accessor :svn_host
    attr_accessor :scp_program
    
    
    attr_accessor :staging
    attr_accessor :dist    
    
    attr_accessor :demo
    
    def initialize(tag)
      init_defaults tag
      yield self if block_given?
      define      
    end
    
    def init_defaults(tag)
      @tag        = tag
      path        = :trunk == tag ? "trunk" : "tags/#{tag}"
      @components = {
        :docs         => "snepo-depot/documentation",
        :flash_client => "snepo-depot/flash-client/#{path}",
        :js_client    => "snepo-depot/javascript-client/#{path}",
        :server       => "snepo-depot/server/#{path}"
      }
      @scp_program = "scp"
      @svn_host    = "svn.snepo.com"
      @svn_proto   = "https"
      @staging     = "./staging"
      @dist        = "./dist"
      @demo        = ENV["DEMO"]
      @deploy_host = "snepo@svn.snepo.com"
    end
    
    def define
      namespace :depot do
        
        desc "Check all distributable depot projects out of SVN"
        task :stage do
          root = Dir::pwd
          begin
            components.each {|component,svn_path|
              mkdir_p staging
              target = "#{staging}/#{component}"
              svn :co, "#{svn_address}/#{svn_path} #{target}"
              begin
                rake_cmd = "rake package"
                rake_cmd += " DEMO=TRUE" if demo
                sh "cd #{target} && #{rake_cmd}"                            
              rescue Object => o
                puts "Error invoking rake on #{component}: #{o}"
              end
            }
          ensure
            cd root
          end
        end
        
        desc "Combines components and bundles installer according to OS"
        task :package do
          mkdir_p dist
          case platform?
            when "win_xp" then package_win
            when "mac_osx" then package_mac
            when "linux" then package_lin
          end
        end
        
        desc "Deploy binary to server"
        task :deploy do
          sh "ssh #{deploy_host} 'mkdir -p #{remote_staging_path}'"
          scp "#{package_name} #{deploy_host}:#{remote_staging_path}"
        end
        
        def package_win
          cp "./nsis/package-win.nsi", "."
          cp "#{staging}/docs/dist/common/depot/license.txt", "./license.txt"
          gather_depot_files_to "#{staging}/win_xp/depot"
          sh '"c:\\Program Files\\NSIS\\makensis.exe" package-win.nsi'          
          mv "install-depot.exe", package_name
          rm "./license.txt"
          rm "./package-win.nsi"
        end
        
        def package_mac
          staging_root = "#{staging}/mac_osx"
          rm_rf staging_root
          sh "sudo rm -rf ./dist/depot-#{output_vsn}.{dmg,pkg}"
          gather_depot_files_to "#{staging_root}/tmp/depot"
          mkdir_p "#{staging_root}/Library/Frameworks"
           
          # now for the pkg bundle
          cp_r "pkg/frameworks/GMP.framework", "#{staging_root}/Library/Frameworks"
          sh "sudo chown -R root:admin #{staging_root}/Library"
          sh "sudo \
/Developer/Applications/Utilities/PackageMaker.app\
/Contents/MacOS/PackageMaker -build -proj \
pkg/depot#{demo ? '-demo' : ''}.pmproj -p ./dist/depot-#{output_vsn}.pkg"
          
          sh "sudo rm -r #{staging_root}/Library" if staging_root && !staging_root.empty?
          # now for the disk image
          sh "sudo hdiutil create -srcfolder ./dist -volname 'Depot#{demo ? ' Demo': ''}' \
#{package_name}"
        end
        
        def package_lin
          gather_depot_files_to "./dist/depot"
          sh "tar -czvf #{package_name} --directory=./dist depot"
        end
        
        def package_name
          case platform?
            when "win_xp" then "#{dist}/depot-#{output_vsn}.exe"
            when "mac_osx" then  "dist/depot-#{output_vsn}.dmg"
            when "linux" then "dist/depot-#{output_vsn}.tar.gz"
          end
        end
        
        def gather_depot_files_to(target)
          puts components.keys.inspect
          mkdir_p                   target
          platform_specific_copy_to target
          common_copy_to            target
        end
        
        def platform_specific_copy_to(dir)
          components.each {|component,ignore|
            files = FileList["#{staging}/#{component}/dist/#{platform}/depot/**/*"]            
            files.each {|from|            
              to = from.sub "#{staging}/#{component}/dist/#{platform}/depot", dir
              if File::directory? from
                mkdir_p to unless File::directory?(to)
              else
                cp from, to
              end
            }
          }          
        end
        
        def common_copy_to(dir)
          components.each{|component,ignore|
            puts component.to_s.upcase
            files = FileList["#{staging}/#{component}/dist/common/depot/**/*"]
            files.each{|from|
              to = from.sub "#{staging}/#{component}/dist/common/depot", dir
              if File::directory? from
                mkdir_p to unless File::directory?(to)
              else
                cp from, to
              end
            }
          }
        end
        
        
        def output_vsn 
          if demo
            "#{tag}-DEMO"
          else
            tag
          end
        end
        
        def remote_staging_path
          "/home/www/apps/depot.snepo.com/store/#{platform}/#{architecture}"
        end
      end
    end
  end
end

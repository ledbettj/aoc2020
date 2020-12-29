#!/usr/bin/env ruby
require 'pry'
require 'set'

INPUT = File.readlines('../inputs/day19').map(&:chomp)

RULE_INPUT = INPUT[0...INPUT.index("")]
CASE_INPUT = INPUT[(INPUT.index("")+1)..]


class D19
  attr_reader :rules

  def matches?(input)
    success, rest = match_rule(input, 0)
    success && rest.any?{ |r| r.length == 0 }
  end

  def rules
    @rules ||= Hash[RULE_INPUT.map do |line|
                      parts = line.split(": ")
                      id = parts[0].to_i
                      options = parts[1].split(' | ').map do |sides|
                        parsed = sides.split(' ').map do |field|
                          field[0] == '"' ? field[1] : field.to_i
                        end
                      end
                      # dont nest A/B
                      if options.length == 1 && options[0].length == 1 && options[0][0].is_a?(String)
                        options = options[0][0]
                      end

                      [id, options]
                    end]
  end

  private

  def match_rule(str, rule_id)
    rule = rules[rule_id]
    if rule.is_a?(String)
      return str[0] == rule ? [true, [str[1..]]] : [false]
    end

    rest = Set.new

    rule.each do |option|
      left = [str]
      option.each do |subrule|
        next_left = Set.new

        left.each do |pos|
          success, possible = match_rule(pos, subrule)
          possible.each { |r| next_left.add(r) } if success
        end
        left = next_left.to_a
      end
      left.each { |item| rest.add(item) }
    end
    return [true, rest.to_a] if rest.any?
    [false]
  end
end

d = D19.new
puts CASE_INPUT.count { |input| d.matches?(input) }

d.rules[8] = [[42],[42, 8]]
d.rules[11] = [[42, 31], [42, 11, 31]]
puts CASE_INPUT.count { |input| d.matches?(input) }

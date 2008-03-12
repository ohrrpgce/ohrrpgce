"""Copyright (c) 2008 James Paige & West Coast Aerospace, Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE."""

import MySQLdb
import ConfigParser
import re

def log(s):
    """default log function is a simple print wrapper. It can
       be replaced by passing another function to the constructor"""
    print s

class MySQL(object):

    def __init__(self, config_file, log_function=log):
        self.log = log_function
        self._database_connect(config_file)

    def _database_connect(self, config_file):
        config = ConfigParser.RawConfigParser()
        config.read(config_file)
        host = config.get('mysql', 'host')
        try:
          port = config.getint('mysql', 'port')
        except ConfigParser.NoOptionError:
          port = 3306
        user = config.get('mysql', 'user')
        passwd = config.get('mysql', 'passwd')
        self.database = config.get('mysql', 'database')
        self.table_prefix = config.get('mysql', 'table_prefix')
        self.db = MySQLdb.connect(host, user, passwd, port=port)

    def exclusive(self, s, compiled_regex):
        """return a copy of a string that only contains chars which
        match a regular expression"""
        result = ""
        for c in s:
            match = compiled_regex.match(c)
            if match != None:
               result += c
        return result

    __table_name_allowed = re.compile('[A-Z0-9\._]', re.I)
    def table_name(self, name):
        """takes a short table name as input and prepends the database
        and table prefix and then sanitizes possible illegal input"""
        s = "%s.%s%s" % (self.database, self.table_prefix, name)
        return self.exclusive(s, self.__table_name_allowed)

    __column_as_alias = re.compile('^(?P<col>.*?) AS (?P<alias>.*)$', re.I)
    __column_name_allowed = re.compile('[A-Z0-9\._]', re.I)
    def column_names(self,column_string):
        """takes a comma-separated list of field names and sanitizes
        them. Understands how to parse 'column AS alias' """
        elements = column_string.split(',')
        result = []
        for element in elements:
            clean = ""
            if element == '*':
              # special case for *
              clean = '*'
            else:
                match = self.__column_as_alias.match(element)
                if match != None:
                    # column AS alias
                    col = match.group('col')
                    alias = match.group('alias')
                    col = self.exclusive(col, self.__column_name_allowed)
                    alias = self.exclusive(alias, self.__column_name_allowed)
                    clean = "%s AS %s" % (col, alias)
                else:
                    # simple column name
                    clean = self.exclusive(element, self.__column_name_allowed)
            result.append(clean)
        return ','.join(result)

    def tables_from_file(self, ini_file):
        raise Error("unimplemented")

    def table(self, table_name):
        return MySQLTable(table_name, self)

    def select(self, from_table, what='*', where=False, limit=False):
        cursor = self.db.cursor()
        query = "SELECT %s from %s" % (self.column_names(what), self.table_name(from_table))
        if where:
            query += " WHERE (%s)" % where
        if limit:
            query += " LIMIT %d" % limit
        self.log(query)
        cursor.execute(query)
        return ResultSet(cursor)

    def delete(self, from_table, where=False, limit=False):
        cursor = self.db.cursor()
        query = "DELETE from %s" % self.table_name(from_table)
        if where:
            query += " WHERE (%s)" % where
        if limit:
            query += " LIMIT %d" % limit
        self.log(query)
        cursor.execute(query)
        return cursor.rowcount

    def insert(self, table, set):
        cursor = self.db.cursor()
        query = "INSERT INTO %s SET %s" % (self.table_name(table), set)
        self.log(query)
        cursor.execute(query)
        return cursor.lastrowid

    def update(self, table, set, where, limit = False):
        cursor = self.db.cursor()
        query = "UPDATE %s SET %s" % (self.table_name(table), set)
        if where:
            query += " WHERE (%s)" % where
        if limit:
            query += " LIMIT %d" % limit
        self.log(query)
        cursor.execute(query)
        return cursor.rowcount

    def describe(self, table):
        cursor = self.db.cursor()
        query = "DESCRIBE %s" % (self.table_name(table))
        self.log(query)
        cursor.execute(query)
        return ResultSet(cursor)

class MySQLTable(object):

    def __init__(self, table_name, db):
        self.name = table_name
        self.db = db

    def select(self, what='*', where=False, limit=False):
        return self.db.select(self.name, what, where, limit)

    def delete(self, where=False, limit=False):
        return self.db.delete(self.name, where, limit)

    def insert(self, set):
        return self.db.insert(self.name, set)

    def update(self, set, where, limit=False):
        return self.db.update(self.name, set, where, limit)

    def describe(self):
        return self.db.describe(self.name)


class ResultSet(object):

    def __init__(self, cursor):
        self.cursor = cursor

    def __iter__(self):
        return self

    def count(self):
      return self.cursor.rowcount

    def next(self):
        row = self.cursor.fetchone()
        if row == None: raise StopIteration
        result = {}
        for i in range(len(row)):
            column_info = self.cursor.description[i]
            name = column_info[0]
            result[name] = row[i]
        return result

/*
The MIT License (MIT)

Copyright (c) 2013 Zack Mulgrew (zackthehuman)

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
THE SOFTWARE.
*/

_ <- (function(root) {

  /**
   * Returns the same value that is used as the argument.
   */
  local identity = function(value) {
    return value;
  };

  /**
   * Retrieve all the names of a table's slots.
   *
   * @param  {Table} table the table to get slots from
   * @return {Array}       an array of all the table's slot names (keys)
   */
  local slots = function(table) {
    local slots = [];

    foreach(key, val in table) {
      slots.push(key);
    }

    return slots;
  };

  /**
   * Return all of the values of the table's slots.
   * @param  {Table} table the table to get values from
   * @return {Array}       an array of all the table's values
   */
  local values = function(table) {
    local values = [];

    foreach(key, val in table) {
      values.push(val);
    }

    return values;
  };

  /**
   * Convert a table into a list of [key, value] pairs.
   *
   * @param  {Table} table the table to create pairs from
   * @return {Array}       an array of [key, value] pairs from the table
   */
  local pairs = function(table) {
    local pairs = [];

    foreach(key, val in table) {
      pairs.push([key, val]);
    }

    return pairs;
  };

  /**
   * Returns a sorted list of the names of every method in an object - that is
   * to say, the name of every function slot of the table.
   * 
   * @param  {Table} table the table to collect function names from
   * @return {Array}       a sorted array of function names
   */
  local functions = function(table) {
    local functionNames = [];

    foreach(key, val in table) {
      if(typeof val == "function") {
        functionNames.push(key);
      }
    }

    functionNames.sort();

    return functionNames;
  };

  /**
   * Copy all of the properties in the source objects over to the destination 
   * object, and return the destination object. It's in-order, so the last 
   * source will override properties of the same name in previous arguments.
   *
   * @param  {[type]} destination [description]
   * @param  {[type]} ...         [description]
   * @return {[type]}             [description]
   */
  local extend = function(destination, ...) {
    if(typeof destination == "table") {
      foreach(source in vargv) {
        if(source != null) {
          foreach(key, val in source) {
            destination[key] <- source[key];
          }
        }
      }
    }

    return destination;
  };

  /**
   * Return a copy of the table, filtered to only have values for the
   * whitelisted slots (or array of valid slots).
   *
   * @param  {Table}        table the table to filter
   * @param  {String|Array} ...   any number of slot names (as strings) or an
   *                              array of slot names (as strings) to pick from 
   *                              table
   * @return {Table}        a new filtered table
   */
  local pick = function(table, ...) {
    local copy = {};

    if(vargv.len() > 0) {
      if(typeof vargv[0] == "array") {
        foreach(key in vargv[0]) {
          if(key in table) {
            copy[key] <- table[key];
          }
        }
      } else {
        foreach(key in vargv) {
          if(key in table) {
            copy[key] <- table[key];
          }
        }
      }
    }

    return copy;
  };
  
  /**
   * Return a copy of the table, filtered to omit the blacklisted slots (or 
   * array of slots).
   *
   * @param  {Table}        table the table to filter
   * @param  {String|Array} ...   any number of slot names (as strings) or an
   *                              array of slot names (as strings) to filter 
   *                              from table
   * @return {Table}        a copy of table with keys removed
   */
  local omit = function(table, ...) {
    local copy = {};
    local keys = {};

    if(vargv.len() > 0) {
      if(typeof vargv[0] == "array") {
        foreach(key in vargv[0]) {
          keys[key] <- true;
        }

        foreach(key, val in table) {
          if(!(key in keys)) {
            copy[key] <- val;
          }
        }
      } else {
        foreach(key in vargv) {
          keys[key] <- true;
        }

        foreach(key, val in table) {
          if(!(key in keys)) {
            copy[key] <- val;
          }
        }
      }
    }

    return copy;
  };

  /**
   * Fill in null and undefined slots in table with values from the defaults
   * tables, and return the table. As soon as the slot is filled, further
   * defaults will have no effect.
   *
   * @param  {[type]} table [description]
   * @param  {[type]} ...   [description]
   * @return {[type]}       [description]
   */
  local defaults = function(table, ...) {
    if(typeof table == "table") {
      foreach(source in vargv) {
        if(source != null) {
          foreach(key, val in source) {
            if(!(key in table) || table[key] == null) {
              table[key] <- source[key];
            }
          }
        }
      }
    }

    return table;
  };

  /**
   * Invokes the given iterator function n times. Each invocation of iterator is
   * called with an `index` argument. An optional context can be provided.
   *
   * @example
   * _.times(3, function(n) { genie.grantWishNumber(n); });
   * // grants all 3 wishes
   *
   * @param  {Number}   n        number of times to execute iterator
   * @param  {Function} iterator the iterator function to call
   * @param  {Table}    context  optional context to invoke the interator under
   */
  local times = function(n, iterator, context = this) {
    local accum = array(n);
    local i;
    
    for(i = 0; i < n; i++) {
      accum[i] = iterator.call(context, i);
    }

    return accum;
  };

  // Export all of the methods
  return {
    slots = slots
    keys = slots
    values = values
    pairs = pairs
    functions = functions
    methods = functions
    extend = extend
    defaults = defaults
    pick = pick
    omit = omit
    times = times
  };

}(getroottable()));
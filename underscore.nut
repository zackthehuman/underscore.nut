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

return (function(root) {

  local _ = {};

  /**
   * Returns the same value that is used as the argument.
   */
  _.identity <- function(value) {
    return value;
  };

  /**
   * Iterates over a list of elements, yielding each in turn to an iterator 
   * function. The iterator is bound to the context object, if one is passed.
   * Each invocation of iterator is called with three arguments: (element, 
   * index, list). If list is a table, iterator's arguments will be (value, key,
   * list).
   *
   * @param  {Array|Table} object   the array or table to iterate
   * @param  {Function}    iterator an iterator to apply to each element
   * @param  {Table}       context  the binding environment
   */
  _.each <- function(object, iterator, context = this) {
    if(typeof object == "array") {
      foreach(idx, val in object) {
        iterator.call(context, val, idx, object);
      }
    } else if(typeof object == "table") {
      foreach(key, val in object) {
        iterator.call(context, val, key, object);
      }
    }
  };

  /**
   * Produces a new array of values by mapping each value in list through a
   * transformation function (iterator). If the native map method exists, it
   * will be used instead. If list is a table, iterator's arguments will be
   * (value, key, list).
   * 
   * @param  {Array}    object   an array of values
   * @param  {Function} iterator the function to be applied to each value
   * @param  {Table}    context  an optional context to bind the function to
   * @return {Array}             a new array filled with the transformed values
   */
  _.map <- function(object, iterator, context = this) {
    local result = [];

    if(typeof object == "array") {
      foreach(idx, val in object) {
        result.push(iterator.call(context, val, idx, object));
      }
    } else if(typeof object == "table") {
      foreach(key, val in object) {
        result.push(iterator.call(context, val, key, object));
      }
    }

    return result;
  };

  /**
   * Also known as inject and foldl, reduce boils down a list of values into a 
   * single value. Memo is the initial state of the reduction, and each 
   * successive step of it should be returned by iterator. The iterator is 
   * passed four arguments: the memo, then the value and index (or key) of the 
   * iteration, and finally a reference to the entire list.
   *
   * @param  {Array}    list     the list of values to reduce
   * @param  {Function} iterator an iterator function to call for each reduction
   * @param  {Value}    memo     the initial value of the reduction
   * @param  {Table}    context  optional context to call iterator with
   * @return {Value}             the vale of list as reduced by iterator
   */
  _.reduce <- _.inject <- _.foldl <- function(list, iterator, memo, context = this) {
    if(list == null) {
      list = [];
    }

    _.each(list, function(value, index, list) {
      memo = iterator.call(context, memo, value, index, list);
    });

    return memo;
  };

  /**
   * Looks through each value in the list, returning the first one that passes a
   * truth test (iterator). The function returns as soon as it finds an 
   * acceptable element, and doesn't traverse the entire list.
   *
   * @param  {Array}    list     an array of values to search through
   * @param  {Function} iterator a search function
   * @param  {Table}    context  an optional context to bind the search function
   * @return {Value}             the found value or null
   */
  _.find <- function(list, iterator, context = this) {
    local result = null;

    foreach(idx, val in list) {
      if(iterator.call(context, val, idx, list)) {
        result = val;
        break;
      }
    }

    return result;
  };

  /**
   * Looks through each value in the list, returning an array of all the values
   * that pass a truth test (iterator).
   *
   * @param  {Array}    list     an array of values to look through
   * @param  {Function} iterator a filter function to use on each value
   * @param  {Table}    context  an optional context to bind the function to
   * @return {Array}             a new array of values that pass the filter
   */
  _.filter <- function(list, iterator, context = this) {
    local results = [];

    foreach(idx, val in list) {
      if(iterator.call(context, val, idx, list)) {
        results.push(val);
      }
    }

    return results;
  };

  /**
   * Looks through each value in the list, returning an array of all the values
   * that contain all of the key-value pairs listed in properties.
   *
   * @param  {Array} list       an array of tables to look through
   * @param  {Table} properties a set of key/value pairs to look for
   * @return {Array}            a new array containing tables that match
   */
  _.where <- function(list, properties) {
    local results = [];

    foreach(item in list) {
      local doesMatch = true;

      foreach(key, val in properties) {
        if(key in item && item[key] != val) {
          doesMatch = false;
          break;
        }
      }

      if(doesMatch) {
        results.push(item);
      }
    }

    return results;
  };

  /**
   * Looks through the list and returns the first value that matches all of the
   * key-value pairs listed in properties.
   *
   * @param  {Array} list       an array of tables to look through
   * @param  {Table} properties a set of key/value pairs to look for
   * @return {Table}            the first value in list matching all properties
   */
  _.findwhere <- _.findWhere <- function(list, properties) {
    foreach(item in list) {
      local doesMatch = true;

      foreach(key, val in properties) {
        if(key in item && item[key] != val) {
          doesMatch = false;
          break;
        }
      }

      if(doesMatch) {
        return item;
      }
    }

    return null;
  };

  /**
   * Returns the values in list without the elements that the truth test 
   * (iterator) passes. The opposite of filter.
   *
   * @param  {Array}    list     the array to search through
   * @param  {Function} iterator "truth test" function used to reject values
   * @param  {Table}    context  an optional context
   * @return {Array}             a new array containing the non-rejected values
   */
  _.reject <- function(list, iterator, context = this) {
    local results = [];

    foreach(idx, val in list) {
      if(!iterator.call(context, val, idx, list)) {
        results.push(val);
      }
    }

    return results;
  };

  /**
   * Returns true if all of the values in the list pass the iterator truth test.
   *
   * @param  {Array}    list     the array to search through
   * @param  {Function} iterator "truth test" function used to check all values
   * @param  {Table}    context  an optional context
   * @return {Boolean}           true if all values pass the truth test
   */
  _.every <- _.all <- function(list, iterator, context = this) {
    local result = true;
    
    if(list == null) {
      return result;
    }

    foreach(idx, val in list) {
      result = result && iterator.call(context, val, idx, list);
      
      if(!result) {
        break;
      }
    }

    return !!result;
  };

  /**
   * Returns true if any of the values in the list pass the iterator truth test.
   * Short-circuits and stops traversing the list if a true element is found.
   *
   * @param  {Array}    list     a list of values to check
   * @param  {Function} iterator a truth text function to check against values
   * @param  {Table}    context  an optional context
   * @return {Boolean}           true if any values in list match the truth test
   */
  _.some <- _.any <- function(list, iterator, context = this) {
    local result = false;

    foreach(idx, val in list) {
      if(iterator.call(context, val, idx, list)) {
        result = true;
        break;
      }
    }

    return result;
  };

  /**
   * Returns true if the value is present in the list. Uses find internally, if
   * list is an Array.
   *
   * @param  {Array|Table} list  an array or table to search in for the value
   * @param  {Value}       value the value to search for
   * @return {Boolean}           true if list contains value, otherwise false
   */
  _.contains <- _.include <- function(list, value) {
    if(typeof list == "array") {
      return list.find(value) != null;
    } else if(typeof list == "table") {
      local values = _.values(list);
      return values.find(value) != null;
    }
  };

  /**
   * Calls the method named by methodName on each value in the list. Any extra
   * arguments passed to invoke will be forwarded on to the method invocation.
   *
   * @param  {Array|Table} list       a list of values to invoke methodName on
   * @param  {String}      methodName the name of a method to call
   * @return {Value}                  the original list after invocations
   */
  _.invoke <- function(list, methodName, ...) {
    local arguments = clone vargv;
    local methodHasArgs = false;

    // Make room for "this"
    arguments.insert(0, null)

    foreach(ixd, val in list) {
      arguments[0] = val;
      val[methodName].acall(arguments);
    }

    return list;
  };

  /**
   * A convenient version of what is perhaps the most common use-case for map: 
   * extracting a list of property values.
   *
   * @param  {Array}  list         an array of tables to search through
   * @param  {String} propertyName the property to look for in each value
   * @return {Array}               an array of the plucked values from each item
   */
  _.pluck <- function(list, propertyName) {
    local results = [];

    foreach(idx, item in list) {
      if(propertyName in item) {
        results.push(item[propertyName]);
      }
    }

    return results;
  };

  /**
   * Returns the maximum value in list. If iterator is passed, it will be used
   * on each value to generate the criterion by which the value is ranked.
   *
   * @param  {Array}    list     the list of values to find the max within
   * @param  {Function} iterator an optional "criteria" function
   * @param  {Table}    context  an optional context to execute iterator with
   * @return {Value}             the maximum value in list
   */
  _.max <- function(list, iterator = _.identity, context = this) {
    local result;

    if(list.len() > 0) {
      result = list[0];

      if(list.len() > 1) {
        foreach(idx, val in list) {
          if(val > result) {
            result = val;
          }
        }
      }
    }

    return result;
  };

  /**
   * Returns the minimum value in list. If iterator is passed, it will be used
   * on each value to generate the criterion by which the value is ranked.
   *
   * @param  {Array}    list     the list of values to find the min within
   * @param  {Function} iterator an optional "criteria" function
   * @param  {Table}    context  an optional context to execute iterator with
   * @return {Value}             the minimum value in list
   */
  _.min <- function(list, iterator = _.identity, context = this) {
    local result;

    if(list.len() > 0) {
      result = list[0];

      if(list.len() > 1) {
        foreach(idx, val in list) {
          if(val < result) {
            result = val;
          }
        }
      }
    }

    return result;
  };

  /**
   * Return the number of values in the list.
   *
   * @param  {Array|Table} list the list to get the size of
   * @return {Integer}          the number of values in list
   */
  _.size <- function(list) {
    if(typeof list == "array") {
      return list.len();
    } else if(typeof list == "table") {
      return _.values(list).len();
    }
  };

  //
  // Array Functions
  //

  /**
   * Returns the first element of an array. Passing n will return the first n
   * elements of the array.
   *
   * @param  {Array}   list the array to get items from
   * @param  {Integer} n    the number of items to get from the front of list
   * @return {Array}        a new array containing the first n items of list
   */
  _.first <- function(list, n = 1) {
    local result = [];

    if(n > 0) {
      if(n > list.len()) {
        n = list.len();
      }

      result = list.slice(0, n);
    }

    return result;
  };

  /**
   * Returns everything but the last entry of the array. Pass n to exclude the
   * last n elements from the result.
   *
   * @param  {Array} list the array to get items from
   * @param  {Integer}    the number if items to exclude from the end of list
   * @return {Array}      a new array containing values not excluded
   */
  _.initial <- function(list, n = 1) {
    local result = [];

    if(n > 0) {
      if(n >= list.len()) {
        n = 0;
      }

      result = list.slice(0, list.len() - n);
    }

    return result;
  };

  /**
   * Returns the last element of an array. Passing n will return the last n
   * elements of the array.
   *
   * @param  {Array}   list the array to get items from
   * @param  {Integer} n    optional number of items to return from end of list
   * @return {Array}        a new array containing the last n items from list
   */
  _.last <- function(list, n = 1) {
    local result = [];

    if(n > 0) {
      if(n > list.len()) {
        n = list.len();
      }
      
      result = list.slice(-n);
    }

    return result;
  };

  /**
   * Returns the rest of the elements in an array. Pass an index to return the
   * values of the array from that index onward.
   *
   * @param  {Array}   list  the array to get items from
   * @param  {Integer} index optional index to start from
   * @return {Array}         all values in list after index
   */
  _.rest <- function(list, index = 1) {
    local result = [];

    if(index < 1) {
      index = 0;
    }

    if(index > list.len()) {
      index = list.len();
    }

    result = list.slice(index);

    return result;
  };

  /**
   * Returns a copy of the array with all falsy values removed. In Squirrel, 
   * null, 0 (integer) and 0.0 (float) are all falsy.
   *
   * @param  {Array} list the list to compact
   * @return {Array}      a new array with falsy values removed
   */
  _.compact <- function(list) {
    local result = [];

    foreach(idx, val in list) {
      if(val) {
        result.push(val);
      }
    }

    return result;
  };

  // Inner recursive function to flatten an array.
  // It needs to be declared first so it can reference itself when it recurses.
  local _flatten = null;

  _flatten = function(list, shallow, output) {
    foreach(idx, val in list) {
      if(_.isarray(val)) {
        if(shallow) {
          output.extend(val);
        } else {
          _flatten(val, shallow, output);
        }
      } else {
        output.push(val);
      }
    }

    return output;
  };

  /**
   * Flattens a nested array (the nesting can be to any depth). If you pass 
   * shallow, the array will only be flattened a single level.
   *
   * @param  {Array}   list    an array to flatten (may contain nested arrays)
   * @param  {Boolean} shallow true if you only want one level deep flattened
   * @return {Array}           a flattened version of list
   */
  _.flatten <- function(list, shallow = false) {
    return _flatten(list, shallow, []);
  };

  /**
   * Returns a copy of the array with all instances of the values removed.
   *
   * @param  {Array} arr the array to remove values from
   * @param  {Value} ... any number of values to exclude
   * @return {Array}     a copy of arr with all specified values removed
   */
  _.without <- function(arr, ...) {
    return _.difference.call(this, arr, vargv);
  };

  /**
   * Converts arrays into tables. Pass either a single list of `[slot, value]`
   * pairs, or a list of slots, and a list of values.
   *
   * @param  {Array} list   array of slot names or array of [slot, value] arrays
   * @param  {Array} values used as array of values if list is only slot names
   * @return {Table}        a new table of merged slots and values
   */
  _.table <- function(list, values = null) {
    if(!list) {
      return {};
    }

    local result = {};

    foreach(idx, val in list) {
      if(values) {
        result[val] <- values[idx];
      } else {
        result[val[0]] <- val[1];
      }
    }

    return result;
  };

  /**
   * Similar to without, but returns the values from array that are not present
   * in the other arrays.
   * 
   * @param  {Array}  arr the original array
   * @param  {Array*} ... one or more arrays containing values to filter out
   * @return {Array}      a new array without any values from other arrays
   */
  _.difference <- function(arr, ...) {
    local rest = [];

    foreach(index, otherArray in vargv) {
      rest.extend(otherArray);
    }

    return _.filter(arr, function(value, ...) {
      return !_.contains(rest, value);
    });
  };

  //
  // Table Functions
  //

  /**
   * Retrieve all the names of a table's slots.
   *
   * @param  {Table} table the table to get slots from
   * @return {Array}       an array of all the table's slot names (keys)
   */
  _.slots <- function(table) {
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
  _.values <- function(table) {
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
  _.pairs <- function(table) {
    local pairs = [];

    foreach(key, val in table) {
      pairs.push([key, val]);
    }

    return pairs;
  };

  /**
   * Returns a copy of the table where the slots have become the values and the
   * values the slots. For this to work, all of your table's values should be
   * unique and string serializable.
   *
   * @param  {Table} table the table to invert
   * @return {Table}       a new table where the keys and values are inverted
   */
  _.invert <- function(table) {
    local result = {};

    foreach(key, val in table) {
      result[val] <- key;
    }

    return result;
  };

  /**
   * Returns a sorted list of the names of every method in an object - that is
   * to say, the name of every function slot of the table.
   * 
   * @param  {Table} table the table to collect function names from
   * @return {Array}       a sorted array of function names
   */
  _.functions <- function(table) {
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
  _.extend <- function(destination, ...) {
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
  _.pick <- function(table, ...) {
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
  _.omit <- function(table, ...) {
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
  _.defaults <- function(table, ...) {
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
   * Create a shallow-copied copy of the object. Any nested tables or arrays
   * will be copied by reference, not duplicated. Exactly the same as using the
   * native clone method.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.copy <- function(object) {
    return clone object;
  };

  /**
   * Invokes interceptor with the table, and then returns table. The primary
   * purpose of this method is to "tap into" a method chain, in order to perform
   * operations on intermediate results within the chain.
   *
   * @param  {[type]} table       [description]
   * @param  {[type]} interceptor [description]
   * @return {[type]}             [description]
   */
  _.tap <- function(table, interceptor) {
    interceptor(table);
    return table;
  };

  /**
   * Returns true if the table contains the given slot.
   *
   * @param  {[type]} table [description]
   * @param  {[type]} slot  [description]
   * @return {[type]}       [description]
   */
  _.has <- function(table, slot) {
    return slot in table;
  };
  
  /**
   * Returns true if collection (table or array) contains no values.
   *
   * @param  {Array|Table} collection the collection to check for emptiness
   * @return {Boolean}                true if collection has no values
   */
  _.isempty <- _.isEmpty <- function(collection) {
    if(typeof collection == "table") {
      return _.slots(collection).len() == 0;
    } else if(typeof collection == "array") {
      return collection.len() == 0;
    }

    return false;
  };

  /**
   * Returns true if object is an Array.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.isarray <- _.isArray <- function(object) {
    return typeof object == "array";
  };

  /**
   * Returns true if object is a Table.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.istable <- _.isTable <- function(object) {
    return typeof object == "table";
  };

  /**
   * Returns true if object is a Function.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.isfunction <- _.isFunction <- function(object) {
    return typeof object == "function";
  };

  /**
   * Returns true if object is a String.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.isstring <- _.isString <- function(object) {
    return typeof object == "string";
  };

  /**
   * Return true if object is an Integer.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.isinteger <- _.isInteger <- function(object) {
    return typeof object == "integer";
  };

  /**
   * Return true if object is a floating-point number (Float).
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.isfloat <- _.isFloat <- function(object) {
    return typeof object == "float";
  };

  /**
   * Returns true if object is a number.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.isnumber <- _.isNumber <- function(object) {
    return isInteger(object) || isFloat(object);
  };

  /**
   * Returns true if object is a Boolean.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.isboolean <- _.isBoolean <- function(object) {
    return typeof object == "boolean";
  };

  /**
   * Returns true if object is Null.
   *
   * @param  {[type]} object [description]
   * @return {[type]}        [description]
   */
  _.isnull <- _.isNull <- function(object) {
    return typeof object == "null";
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
  _.times <- function(n, iterator, context = this) {
    local accum = array(n);
    local i;
    
    for(i = 0; i < n; i++) {
      accum[i] = iterator.call(context, i);
    }

    return accum;
  };
  
  return _;

}(getroottable()));
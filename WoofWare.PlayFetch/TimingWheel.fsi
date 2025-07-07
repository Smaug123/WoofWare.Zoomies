namespace WoofWare.PlayFetch

[<Struct>]
type IntervalNum =
    {
        Span : int64
    }

[<RequireQualifiedAccess>]
module internal IntervalNum =
    type Span

    [<RequireQualifiedAccess>]
    module Span =
        val max : Span -> Span -> Span
        val zero : Span
        val one : Span
        val maxValue : Span
        val ofInt64 : int64 -> Span
        val toInt64 : Span -> int64
        val ofInt : int -> Span
        val toIntThrowing : Span -> int
        val pred : Span -> Span
        val succ : Span -> Span
        val add : Span -> Span -> Span
        val scale : Span -> int -> Span

    val max : IntervalNum -> IntervalNum -> IntervalNum
    val min : IntervalNum -> IntervalNum -> IntervalNum
    val zero : IntervalNum
    val one : IntervalNum
    val minValue : IntervalNum
    val maxValue : IntervalNum
    val ofInt64 : int64 -> IntervalNum
    val toInt64 : IntervalNum -> int64
    val ofInt : int -> IntervalNum
    val toIntThrowing : IntervalNum -> int
    val pred : IntervalNum -> IntervalNum
    val succ : IntervalNum -> IntervalNum
    val add : IntervalNum -> IntervalNum -> IntervalNum
    val sub : IntervalNum -> IntervalNum -> IntervalNum
    val diff : IntervalNum -> IntervalNum -> IntervalNum
    val rem : IntervalNum -> Span -> Span

type AlarmPrecision

[<RequireQualifiedAccess>]
module AlarmPrecision =
    val ofSpanFloorPow2Ns : System.TimeSpan -> AlarmPrecision
    val toSpan : AlarmPrecision -> System.TimeSpan
    val oneNanosecond : AlarmPrecision
    ///  ~19.5 h
    val aboutOneDay : AlarmPrecision
    /// 1.024 us
    val aboutOneMicrosecond : AlarmPrecision

    /// ~1.05 ms
    val aboutOneMillisecond : AlarmPrecision

    /// ~1.07 s
    val aboutOneSecond : AlarmPrecision

    /// [mul t ~pow2] is [t * 2^pow2]. [pow2] may be negative, but [mul] does not check for
    /// overflow or underflow.
    val mul : AlarmPrecision -> pow2 : int -> AlarmPrecision

    /// [div t ~pow2] is [t / 2^pow2]. [pow2] may be negative, but [div] does not check for
    /// overflow or underflow.
    val div : AlarmPrecision -> pow2 : int -> AlarmPrecision

type TimingWheel<'a>

[<RequireQualifiedAccess>]
module TimingWheel =
    type Alarm<'a>

    module Alarm =
        val nullAlarm<'a> : unit -> Alarm<'a>
        val at<'a> : 'a TimingWheel -> 'a Alarm -> TimeNs
        val intervalNum : 'a TimingWheel -> 'a Alarm -> IntervalNum
        val value : 'a TimingWheel -> 'a Alarm -> 'a

    type LevelBits

    module LevelBits =
        /// [max_num_bits] is how many bits in a key the timing wheel can use, i.e. 61. We
        ///    subtract 3 for the bits in the word that we won't use:
        ///    - for the tag bit
        ///    - for negative numbers
        ///    - so we can do arithmetic around the bound without worrying about overflow
        val maxNumBits : int

        /// In [create_exn bits], it is an error if any of the [b_i] in [bits] has [b_i <= 0],
        ///    or if the sum of the [b_i] in [bits] is greater than [max_num_bits]. With
        ///    [~extend_to_max_num_bits:true], the resulting [t] is extended with sufficient
        ///    [b_i = 1] so that [num_bits t = max_num_bits].
        /// Default extendToMaxNumBits is false.
        val createThrowing
          :  ?extendToMaxNumBits:bool
          -> int list
          -> LevelBits

        /// [default'] returns the default value of [level_bits] used by [Timing_wheel.create]
        ///    and [Timing_wheel.Priority_queue.create].
        ///    {[
        ///      default = [ 11; 10; 10; 10; 10; 10 ]
        ///    ]}
        ///    This default uses 61 bits, i.e. [max_num_bits], and less than 10k words of memory.
        val default' : LevelBits

        /// [num_bits t] is the sum of the [b_i] in [t].
        val numBits : LevelBits -> int

    type Config

    module Config =
        /// [create] raises if [alarm_precision <= 0].
        /// default capacity is 1
        val create
          :  capacity:int option
          -> levelBits:LevelBits option
          -> alarmPrecision:AlarmPrecision
          -> unit
          -> Config

        val alarmPrecision : Config -> System.TimeSpan

        val levelBits : Config -> LevelBits

        /// [durations t] returns the durations of the levels in [t]
        val durations : Config -> System.TimeSpan list

        /// [microsecond_precision ()] returns a reasonable configuration for a timing wheel
        ///    with microsecond [alarm_precision], and level durations of 1ms, 1s, 1m, 1h, 1d.
        val microsecondPrecision : unit -> Config

    /// [create ~config ~start] creates a new timing wheel with current time [start].
    ///    [create] raises if [start < Time_ns.epoch]. For a fixed [level_bits], a smaller
    ///    (i.e. more precise) [alarm_precision] decreases the representable range of
    ///    times/keys and increases the constant factor for [advance_clock].
    val create : config:Config -> start:TimeNs -> 'a TimingWheel

    val alarmPrecision : 'a TimingWheel -> System.TimeSpan

    val now : 'a TimingWheel -> TimeNs
    val start : 'a TimingWheel -> TimeNs

    val isEmpty : 'a TimingWheel -> bool
    val length : _ TimingWheel -> int
    val iter : 'a TimingWheel -> f:('a Alarm -> unit) -> unit

    /// [interval_num t time] returns the number of the interval that [time] is in, where
    ///  [0] is the interval that starts at [Time_ns.epoch]. [interval_num] raises if
    ///  [Time_ns.( < ) time Time_ns.epoch].
    val intervalNum : 'a TimingWheel -> TimeNs -> IntervalNum

    /// [now_interval_num t = interval_num t (now t)].
    val nowIntervalNum : 'a TimingWheel -> IntervalNum

  (** [interval_num_start t n] is the start of the [n]'th interval in [t], i.e.
      [n * alarm_precision t] after the epoch.

      [interval_start t time] is the start of the half-open interval containing [time],
      i.e.:

      {[
        interval_num_start t (interval_num t time)
      ]} *)
    val intervalNumStart : 'a TimingWheel -> IntervalNum -> TimeNs

    /// [interval_start] raises in the same cases that [interval_num] does.
    val intervalStart : 'a TimingWheel -> TimeNs -> TimeNs

  (** [advance_clock t ~to_ ~handle_fired] advances [t]'s clock to [to_]. It fires and
      removes all alarms [a] in [t] with
      [Time_ns.(<) (Alarm.at t a) (interval_start t to_)], applying [handle_fired] to each
      such [a].

      If [to_ <= now t], then [advance_clock] does nothing.

      [advance_clock] fails if [to_] is too far in the future to represent.

      Behavior is unspecified if [handle_fired] accesses [t] in any way other than [Alarm]
      functions. *)
    val advanceClock : 'a TimingWheel -> to_:TimeNs -> handle_fired:('a Alarm -> unit) -> unit

  (** Advance to the time [to_] or the time of the next alarm, whichever is earlier. This
      function should be functionally equivalent to
      [advance_clock t ~to_:(Time.min to_ (min_alarm_time_in_min_interval t))], with
      potentially better performance.

      [handle_fired] may still fire multiple times, if there are multiple alarms scheduled
      at the same time. *)
    val advanceClockStopAtNextAlarm
      :  'a TimingWheel
      -> to_:TimeNs
      -> handle_fired:('a Alarm -> unit)
      -> unit

  (** [fire_past_alarms t ~handle_fired] fires and removes all alarms [a] in [t] with
      [Time_ns.( <= ) (Alarm.at t a) (now t)], applying [handle_fired] to each such [a].

      [fire_past_alarms] visits all alarms in interval [now_interval_num], to check their
      [Alarm.at].

      Behavior is unspecified if [handle_fired] accesses [t] in any way other than [Alarm]
      functions. *)
    val firePastAlarms : 'a TimingWheel -> handle_fired:('a Alarm -> unit) -> unit

  (** [max_allowed_alarm_time t] returns the greatest [at] that can be supplied to [add].
      [max_allowed_alarm_time] is not constant; its value increases as [now t] increases. *)
    val maxAllowedAlarmTime : _ TimingWheel -> TimeNs

  (** [min_allowed_alarm_interval_num t = now_interval_num t] *)
    val minAllowedAlarmIntervalNum : _ TimingWheel -> IntervalNum

  (** [max_allowed_alarm_interval_num t = interval_num t (max_allowed_alarm_time t)] *)
    val maxAllowedAlarmIntervalNum : _ TimingWheel -> IntervalNum

  (** [add t ~at a] adds a new value [a] to [t] and returns an alarm that can later be
      supplied to [remove] the alarm from [t]. [add] raises if
      [interval_num t at < now_interval_num t || at > max_allowed_alarm_time t]. *)
    val add : 'a TimingWheel -> at:TimeNs -> 'a -> 'a Alarm

  (** [add_at_interval_num t ~at a] is equivalent to
      [add t ~at:(interval_num_start t at) a]. *)
    val addAtIntervalNum : 'a TimingWheel -> at:IntervalNum -> 'a -> 'a Alarm

    val mem : 'a TimingWheel -> 'a Alarm -> bool

  (** [remove t alarm] removes [alarm] from [t]. [remove] raises if [not (mem t alarm)]. *)
    val remove : 'a TimingWheel -> 'a Alarm -> unit

  (** [reschedule t alarm ~at] mutates [alarm] so that it will fire at [at], i.e. so that
      [Alarm.at t alarm = at]. [reschedule] raises if [not (mem t alarm)] or if [at] is an
      invalid time for [t], in the same situations that [add] raises. *)
    val reschedule : 'a TimingWheel -> 'a Alarm -> at:TimeNs -> unit

  (** [reschedule_at_interval_num t alarm ~at] is equivalent to:
      {[
        reschedule t alarm ~at:(interval_num_start t at)
      ]} *)
    val rescheduleAtIntervalNum : 'a TimingWheel -> 'a Alarm -> at:IntervalNum -> unit

  (** [clear t] removes all alarms from [t]. *)
    val clear : _ TimingWheel -> unit

  (** [min_alarm_interval_num t] is the minimum [Alarm.interval_num] of all alarms in [t]. *)
    val minAlarmIntervalNum : _ TimingWheel -> IntervalNum option

  (** [min_alarm_interval_num_exn t] is like [min_alarm_interval_num], except it raises if
      [is_empty t]. *)
    val minAlarmIntervalNumThrowing : _ TimingWheel -> IntervalNum

  (** [max_alarm_time_in_min_interval t] returns the maximum [Alarm.at] over all alarms in
      [t] whose [Alarm.interval_num] is [min_alarm_interval_num t]. This function is
      useful for advancing to the [min_alarm_interval_num] of a timing wheel and then
      calling [fire_past_alarms] to fire the alarms in that interval. That is useful when
      simulating time, to ensure that alarms are processed in order. *)
    val maxAlarmTimeInMinInterval : 'a TimingWheel -> TimeNs option

  (** [min_alarm_time_in_min_interval t] returns the minimum [Alarm.at] over all alarms in
      [t]. This function is useful for advancing to the exact time when the next alarm is
      scheduled to fire. *)
    val minAlarmTimeInMinInterval : 'a TimingWheel -> TimeNs option

  (** [max_alarm_time_in_min_interval_exn t] is like [max_alarm_time_in_min_interval],
      except that it raises if [is_empty t]. *)
    val maxAlarmTimeInMinIntervalThrowing : 'a TimingWheel -> TimeNs

  (** [min_alarm_time_in_min_interval_exn t] is like [min_alarm_time_in_min_interval],
      except that it raises if [is_empty t]. *)
    val minAlarmTimeInMinIntervalThrowing : 'a TimingWheel -> TimeNs

  (** The name of this function is misleading: it does not take into account events that
      can fire due to [fire_past_alarms].

      [next_alarm_fires_at t] returns the minimum time to which the clock can be advanced
      such that an alarm will be fired by [advance_clock], or [None] if [t] has no alarms
      (or all alarms are in the max interval, and hence cannot fire by [advance_clock]).
      If [next_alarm_fires_at t = Some next], then for the minimum alarm time [min] that
      occurs in [t], it is guaranteed that: [next - alarm_precision t <= min < next]. *)
    val nextAlarmFiresAt : _ TimingWheel -> TimeNs option

  (** [next_alarm_fires_at_exn] is like [next_alarm_fires_at], except that it raises if
      [is_empty t]. *)
    val nextAlarmFiresAtThrowing : _ TimingWheel -> TimeNs

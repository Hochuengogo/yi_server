CREATE TABLE `role_base` (
  `rid` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '角色ID',
  `srv_id` char(30) NOT NULL COMMENT '服务器ID',
  `account` char(100) NOT NULL COMMENT '账号',
  `name` char(20) NOT NULL COMMENT '角色名',
  `career` tinyint(4) unsigned NOT NULL DEFAULT '0' COMMENT '职业',
  `type` tinyint(4) unsigned NOT NULL DEFAULT '0' COMMENT '角色类型 0:普通玩家 1:GM 2:新手指导员',
  `sex` tinyint(4) unsigned NOT NULL DEFAULT '0' COMMENT '性别 0:无性别 1:男 2:女',
  `face_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '头像ID',
  `lev` smallint(6) unsigned NOT NULL DEFAULT '1' COMMENT '等级',
  `vip_lev` smallint(6) unsigned NOT NULL DEFAULT '0' COMMENT 'VIP等级',
  `power` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '战力',
  `max_power` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '最高战力',
  `login_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '最后登录时间',
  `logout_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '最后登出时间',
  `login_ip` char(15) NOT NULL DEFAULT '' COMMENT '登录IP',
  `is_online` tinyint(4) unsigned NOT NULL DEFAULT '0' COMMENT '是否在线 0:不在线 1:在线',
  `banned_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '封号时间',
  `data_lock_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '数据锁定时间',
  `identity` char(20) NOT NULL DEFAULT '' COMMENT '身份证号',
  `reg_channel` char(32) NOT NULL DEFAULT '' COMMENT '注册渠道',
  `channel` char(32) NOT NULL DEFAULT '' COMMENT '渠道',
  `reg_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '注册时间',
  `reg_ip` char(15) NOT NULL DEFAULT '' COMMENT '注册IP',
  `reg_idfa` varchar(64) NOT NULL DEFAULT '' COMMENT '注册设备号',
  `idfa` varchar(64) NOT NULL DEFAULT '' COMMENT '设备号',
  `reg_device_id` varchar(64) NOT NULL DEFAULT '' COMMENT '注册设备ID',
  `device_id` varchar(64) NOT NULL DEFAULT '' COMMENT '设备ID',
  `reg_os_name` varchar(32) NOT NULL DEFAULT '' COMMENT '注册设备系统',
  `os_name` varchar(32) NOT NULL DEFAULT '' COMMENT '设备系统',
  `reg_package_name` varchar(64) NOT NULL DEFAULT '' COMMENT '注册包',
  `package_name` varchar(64) NOT NULL DEFAULT '' COMMENT '包',
  `reg_package_version` varchar(22) NOT NULL DEFAULT '' COMMENT '注册包版本',
  `package_version` varchar(22) NOT NULL DEFAULT '' COMMENT '包版本',
  `platform` char(32) NOT NULL COMMENT '平台',
  PRIMARY KEY (`rid`,`srv_id`),
  UNIQUE KEY `name` (`name`),
  KEY `srv_id` (`srv_id`),
  KEY `account` (`account`),
  KEY `career` (`career`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='角色基础信息表';

CREATE TABLE `role_data` (
  `rid` int(11) unsigned NOT NULL COMMENT '角色ID',
  `srv_id` char(30) NOT NULL COMMENT '服务器ID',
  `data` mediumblob NOT NULL COMMENT '角色详细数据',
  PRIMARY KEY (`rid`,`srv_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='角色数据表';

CREATE TABLE `role_assets` (
  `rid` int(11) unsigned NOT NULL COMMENT '角色ID',
  `srv_id` char(30) NOT NULL COMMENT '服务器ID',
  `gold_acc` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '累充钻石',
  `gold` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '钻石',
  `coin` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '金币',
  PRIMARY KEY (`rid`,`srv_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='角色资产表';

CREATE TABLE `charge` (
  `order_id` int(11) unsigned NOT NULL COMMENT '订单ID',
  `rid` int(11) unsigned NOT NULL COMMENT '角色ID',
  `srv_id` char(30) NOT NULL COMMENT '服务器ID',
  `money` decimal(13,3) unsigned NOT NULL COMMENT '金额',
  `money_type` smallint(6) unsigned NOT NULL COMMENT '金钱类型',
  `timestamp` int(11) unsigned NOT NULL COMMENT '充值时间',
  `channel` char(32) NOT NULL COMMENT '充值渠道',
  `charge_id` int(11) unsigned NOT NULL COMMENT '充值ID',
  PRIMARY KEY (`order_id`),
  KEY `role_id` (`rid`,`srv_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='充值表';
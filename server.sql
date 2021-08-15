CREATE TABLE IF NOT EXISTS `role_data` (
    `role_id` INT UNSIGNED NOT NULL COMMENT '角色ID',
    `srv_id` VARCHAR(20) NOT NULL COMMENT '服务器ID',
    `data` LONGBLOB NOT NULL COMMENT '角色数据',
    PRIMARY KEY(`role_id`, `srv_id`)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT '角色表';

CREATE TABLE IF NOT EXISTS `role_base` (
    `role_id` INT UNSIGNED NOT NULL COMMENT '角色ID',
    `srv_id` VARCHAR(20) NOT NULL COMMENT '服务器ID',
    `name` VARCHAR(20) NOT NULL COMMENT '角色名',
    `sex` TINYINT NOT NULL COMMENT '性别 0:女 1:男',
    `career` TINYINT UNSIGNED NOT NULL COMMENT '职业 0:无职业 其他:按情况定义',
    `account` VARCHAR(50) NOT NULL COMMENT '账号',
    PRIMARY KEY(`role_id`, `srv_id`),
    UNIQUE INDEX(`name`)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT '角色基础数据表';

CREATE TABLE IF NOT EXISTS `assets` (
    `role_id` INT UNSIGNED NOT NULL COMMENT '角色ID',
    `srv_id` VARCHAR(20) NOT NULL COMMENT '服务器ID',
    `coin` INT UNSIGNED NOT NULL DEFAULT 0 COMMENT '金币',
    `gold` INT UNSIGNED NOT NULL DEFAULT 0 COMMENT '钻石',
    `voucher` INT UNSIGNED NOT NULL DEFAULT 0 COMMENT '点券',
    PRIMARY KEY(`role_id`, `srv_id`)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT '角色重要资产表';

CREATE TABLE IF NOT EXISTS `charge` (
    `order_id` INT UNSIGNED NOT NULL COMMENT '订单号',
    `role_id` INT UNSIGNED NOT NULL COMMENT '角色ID',
    `srv_id` VARCHAR(20) NOT NULL COMMENT '服务器ID',
    `money` DECIMAL(13,3) UNSIGNED NOT NULL COMMENT '金额',
    `timestamp` INT UNSIGNED NOT NULL COMMENT '时间戳',
    `channel` SMALLINT UNSIGNED NOT NULL COMMENT '充值渠道',
    `charge_id` INT UNSIGNED NOT NULL COMMENT '充值包ID',
    PRIMARY KEY(`order_id`),
    INDEX (`role_id`, `srv_id`)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT '充值表';
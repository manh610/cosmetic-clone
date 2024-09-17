package com.cosmetic.gg.entity.attribute;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Index;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "attribute", indexes = {
    @Index(name = "idx_attribute_code", columnList = "code"),
    @Index(name = "idx_attribute_name", columnList = "name")
})
@Entity
@Getter
@Setter
public class Attribute extends EntityBase{

	@NotNull(message = "Code can not null")
	@Column(name = "code", unique = true)
	private String code;
	
	@NotNull(message = "Name can not null")
	@Column(name = "name")
	private String name;
	
	@Column(name = "description")
	private String description;
	
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private EStatus status;
}

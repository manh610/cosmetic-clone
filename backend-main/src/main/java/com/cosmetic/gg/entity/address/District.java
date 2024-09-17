package com.cosmetic.gg.entity.address;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "districts")
@Entity
@Getter
@Setter
public class District extends EntityBase{
	private static final long serialVersionUID = 1L;

	@NotNull(message = "District name can not null")
	@Column(name = "name")
	private String name;
	
	@NotNull(message = "District fullname can not null")
	@Column(name = "full_name")
	private String fullName;
	
	@NotNull(message = "Id of province can not null")
	@Column(name = "province_id")
	private String provinceId;
}

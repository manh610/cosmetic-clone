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
@Table(name = "wards")
@Entity
@Getter
@Setter
public class Ward extends EntityBase {
	private static final long serialVersionUID = 1L;

	@NotNull(message = "Ward name can not null")
	@Column(name = "name")
	private String name;
	
	@NotNull(message = "Ward fullname can not null")
	@Column(name = "full_name")
	private String fullName;
	
	@NotNull(message = "Id of district can not null")
	@Column(name = "district_id")
	private String districtId;
}

package com.cosmetic.gg.entity.address;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "user_address")
@Entity
@Getter
@Setter
public class UserAddress extends EntityBase {
	
	@NotEmpty(message = "Id of user can not empty")
	@NotNull(message = "Id of user can not null")
	@Column(name = "user_id", length = 700)
	private String userId;
	
	@NotEmpty(message = "Id of address can not empty")
	@NotNull(message = "Id of address can not null")
	@Column(name = "address_id", length = 700)
	private String addressId;
}
